#include <assert.h>
#include "util.hpp"
#include "bytecode/opcode.hpp"
#include "bytecode/emitter.hpp"
#include "bytecode/compiler.hpp"
#include "bytecode/disassemble.hpp"
#include "vm_state.hpp"

#define TYPE_CHECK(what, typecheck, expected)                           \
    do {                                                                \
        if (!(what).typecheck) {                                        \
            signal_args = gc.list(g.s_TYPE_ERROR, (expected), (what));  \
            goto raise_signal;                                          \
        }                                                               \
    } while (0)

using namespace lisp;

template<bool debuggable>
const uint8_t *VM_State::execute_impl(const uint8_t *ip)
{

#define BYTECODE_DEF(name, noperands, nargs, size, docstring) &&opcode_ ## name,
    void *computed_gotos[256] =
    {
        #include "bytecode.def"
    };

#define DISPATCH(name) case bytecode::Opcode::op_ ## name: opcode_ ## name:

#define DISPATCH_NEXT if constexpr (debuggable) break; else goto *computed_gotos[*ip];
#define EXEC switch(static_cast<bytecode::Opcode>(*ip))
#define DISPATCH_LOOP for (;;)

#if PROFILE_OPCODE_PAIRS
#define PREDICTED(name) //empty
#define PREDICT(name) //empty
#else
#define PREDICTED(name) predicted_ ## name:
#define PREDICT(name)                                                 \
    do {                                                                \
        if (static_cast<bytecode::Opcode>(*ip) == bytecode::Opcode::op_ ## name) \
        {                                                               \
            goto predicted_ ## name;                                       \
        }                                                               \
    } while (0)
#endif

    static_assert(sizeof(*ip) == 1, "pointer arithmetic will not work as expected.");
    Value signal_args;
    Value func;
    uint32_t nargs;
    enum class Call_Type
    {
        Doesnt_Push_Frame,
        Pushes_Frame,
    } call_type;
    bool with_return = false;
    DISPATCH_LOOP
    {
        if constexpr (debuggable)
        {
            const auto opcode = static_cast<bytecode::Opcode>(*ip);
            if (g.debugger.breaking)
            {
                // @TODO: Fix debugger step over:
                // currently it just goes to the next instruction address which is ok in the case of
                // op_apply or op_funcall but it is incorrect in the case off op_jump and op_pop_jump_if_nil
                if ((g.debugger.command == Runtime_Globals::Debugger::Command::Step_Over &&
                     (g.debugger.addr0 == ip || g.debugger.addr1 == ip))
                    || g.debugger.command == Runtime_Globals::Debugger::Command::Step_Into)
                {
                    debug_dump(std::cout, "VM EXEC", ip);
                    auto &in = std::cin;
                    bool eat_newline = true;
                    switch (in.peek())
                    {
                        case 'c':
                            g.debugger.command = Runtime_Globals::Debugger::Command::Continue;
                            in.get();
                            break;
                        case 's':
                            g.debugger.command = Runtime_Globals::Debugger::Command::Step_Into;
                            in.get();
                            break;
                        case 'n':
                            g.debugger.command = Runtime_Globals::Debugger::Command::Step_Over;
                            in.get();
                            break;
                        case '\n':
                            in.get();
                            eat_newline = false;
                            break;
                    }

                    if (eat_newline && in.peek() == '\n')
                    {
                        in.get();
                    }

                    switch (g.debugger.command)
                    {
                        case Runtime_Globals::Debugger::Command::Continue:
                            g.debugger.breaking = false;
                            break;
                        case Runtime_Globals::Debugger::Command::Step_Into:
                            break;
                        case Runtime_Globals::Debugger::Command::Step_Over:
                            g.debugger.addr0 = ip + bytecode::opcode_size(opcode);
                            g.debugger.addr1 = nullptr;
                            if (opcode == bytecode::Opcode::op_pop_jump_if_nil)
                            {
                                auto offset = *reinterpret_cast<const int32_t*>(ip+1);
                                g.debugger.addr1 = ip + offset;
                            }
                            else if (opcode == bytecode::Opcode::op_jump)
                            {
                                auto offset = *reinterpret_cast<const int32_t*>(ip+1);
                                g.debugger.addr0 = ip + offset;
                            }
                            break;
                    }
                }
            }
#if PROFILE_OPCODE_PAIRS
            auto this_opcode = static_cast<bytecode::Opcode>(*ip);
            switch (this_opcode)
            {
                case bytecode::Opcode::op_halt:
                case bytecode::Opcode::op_return:
                case bytecode::Opcode::op_apply:
                case bytecode::Opcode::op_funcall:
                case bytecode::Opcode::op_gotocall:
                case bytecode::Opcode::op_jump:
                case bytecode::Opcode::op_pop_jump_if_nil:
                    break;
                default: {
                    auto next_opcode = *(ip + bytecode::opcode_size(this_opcode));
                    m_opcode_pairs[Opcode_Pair{static_cast<short>(this_opcode), next_opcode}]++;
                } break;
            }
#endif
        } // if constexpr (debuggable)

        EXEC
        {
            DISPATCH(apply)
            {
                func = pop_param();
                nargs = *reinterpret_cast<const uint32_t*>(ip+1);
                if (nargs == 0)
                {
                    GC_GUARD();
                    signal_args = gc.list(g.s_SIMPLE_ERROR, gc.alloc_string("Too few arguments!"));
                    GC_UNGUARD();
                    goto raise_signal;
                }

                auto last_arg = pop_param();
                CHECK_LIST(last_arg);
                nargs--;
                while (!last_arg.is_nil())
                {
                    push_param(car(last_arg));
                    last_arg = cdr(last_arg);
                    nargs++;
                }
                call_type = Call_Type::Pushes_Frame;
                with_return = false;
                goto do_funcall;
            }

            DISPATCH(funcall)
            {
                PREDICTED(funcall);
                func = pop_param();
                nargs = *reinterpret_cast<const uint32_t*>(ip+1);
                call_type = Call_Type::Pushes_Frame;
                with_return = false;

                do_funcall:
                auto ofunc = func;
                if (symbolp(func))
                {
                    func = func.as_object()->symbol()->function();
                }

                if (func.is_lisp_primitive())
                {
                    bool raised_signal = false;
                    auto primitive = func.as_lisp_primitive();
                    auto result = primitive(m_stack_top - nargs, nargs, raised_signal);
                    m_stack_top -= nargs;
                    if (raised_signal)
                    {
                        signal_args = result;
                        goto raise_signal;
                    }
                    else
                    {
                        push_param(result);
                        ip += 1 + sizeof(nargs);
                    }
                    if (with_return)
                    {
                        goto do_return;
                    }
                    else
                    {
                        DISPATCH_NEXT;
                    }
                }

                if (func.is_type(Object_Type::Closure))
                {
                    // Although op_raise_signal also jumps here in a "funcall"-like way, it has
                    // already setup the stack in the state the closure expects to execute under
                    // so there is no need to push a frame for it here.
                    if (call_type == Call_Type::Pushes_Frame)
                    {
                        push_frame(ip+5, nargs);
                    }

                    m_current_closure = func;
                    auto closure = func.as_object()->closure();
                    auto function = closure->function();

                    // locals always start at first argument
                    m_locals = m_stack_top - nargs;

                    if (function->is_too_many_args(nargs))
                    {
                        GC_GUARD();
                        signal_args = gc.list(g.s_SIMPLE_ERROR,
                                              gc.alloc_string("Too many arguments!"),
                                              func,
                                              Value::wrap_fixnum(function->arity()),
                                              Value::wrap_fixnum(nargs));
                        GC_UNGUARD();
                        goto raise_signal;
                    }

                    if (function->is_too_few_args(nargs))
                    {
                        GC_GUARD();
                        signal_args = gc.list(g.s_SIMPLE_ERROR,
                                              gc.alloc_string("Too few arguments!"),
                                              func,
                                              Value::wrap_fixnum(function->arity()),
                                              Value::wrap_fixnum(nargs));
                        GC_UNGUARD();
                        goto raise_signal;
                    }

                    if (function->has_rest() && nargs > function->rest_index())
                    {
                        auto rest = to_list(m_locals+function->rest_index(),
                                            nargs - function->rest_index());
                        m_locals[function->rest_index()] = rest;
                    }

                    m_stack_top = m_locals + function->num_locals();
                    #if DEBUG > 1
                    {
                        assert(function->arity() <= function->num_locals());
                        auto start = m_locals + function->arity();
                        auto end = m_locals + function->num_locals();
                        for (; start != end; ++start)
                        {
                            *start = Value::nil();
                        }
                    }
                    #endif

                    ip = function->entrypoint(nargs);
                    DISPATCH_NEXT;
                }

                // error
                GC_GUARD();
                signal_args = gc.list(g.s_SIMPLE_ERROR, gc.alloc_string("Not a callable object"), ofunc, func);
                GC_UNGUARD();
                goto raise_signal;
            }

            DISPATCH(gotocall)
            {

                close_values(m_locals);

                func = pop_param();
                nargs = *reinterpret_cast<const uint32_t*>(ip+1);
                auto begin = m_stack_top - nargs;
                auto end = m_stack_top;
                m_stack_top = m_locals;
                std::copy(begin, end, m_stack_top);
                m_stack_top += nargs;

                call_type = Call_Type::Doesnt_Push_Frame;
                with_return = true;
                goto do_funcall;
            }

            DISPATCH(pop_handler_case)
                pop_handler_case();
                // fallthrough
            DISPATCH(return)
            {
                do_return:
                if (m_call_frame_top == m_call_frame_bottom)
                {
                    goto done;
                }

                close_values(m_locals);

                auto val = param_top();
                auto frame = pop_frame();
                set_frame(frame);
                push_param(val);
                if (frame.ip == nullptr)
                {
                    goto done;
                }
                ip = frame.ip;
                DISPATCH_NEXT;
            }

            DISPATCH(jump)
            {
                auto offset = *reinterpret_cast<const int32_t*>(ip+1);
                ip += offset;
                DISPATCH_NEXT;
            }

            DISPATCH(pop_jump_if_nil)
            {
                if (pop_param().is_nil())
                {
                    auto offset = *reinterpret_cast<const int32_t*>(ip+1);
                    ip += offset;
                }
                else
                {
                    ip += 5;
                }
                DISPATCH_NEXT;
            }

            DISPATCH(get_global)
            {
                auto index = *reinterpret_cast<const uint32_t*>(ip+1);
                push_param(g.global_value_slots[index]);
                ip += 5;
                DISPATCH_NEXT;
            }

            DISPATCH(set_global)
            {
                auto index = *reinterpret_cast<const uint32_t*>(ip+1);
                g.global_value_slots[index] = param_top();
                ip += 5;
                PREDICT(pop);
                DISPATCH_NEXT;
            }

            DISPATCH(get_local)
            {
                PREDICTED(get_local);
                auto index = *reinterpret_cast<const uint32_t*>(ip+1);
                push_param(m_locals[index]);
                ip += 5;
                PREDICT(push_value);
                PREDICT(get_local);
                DISPATCH_NEXT;
            }

            DISPATCH(set_local)
            {
                PREDICTED(set_local)
                auto index = *reinterpret_cast<const uint32_t*>(ip+1);
                m_locals[index] = param_top();
                ip += 5;
                PREDICT(pop);
                DISPATCH_NEXT;
            }

            DISPATCH(get_capture)
            {
                auto index = *reinterpret_cast<const uint32_t*>(ip+1);
                push_param(m_current_closure.as_object()->closure()->get_capture(index));
                ip += 5;
                DISPATCH_NEXT;
            }

            DISPATCH(set_capture)
            {
                auto index = *reinterpret_cast<const uint32_t*>(ip+1);
                m_current_closure.as_object()->closure()->set_capture(index, param_top());
                ip += 5;
                PREDICT(pop);
                DISPATCH_NEXT;
            }

            DISPATCH(function_value)
            {
                auto obj = *reinterpret_cast<const Value*>(ip+1);
                if (symbolp(obj))
                {
                    push_param(obj.as_object()->symbol()->function());
                }
                else
                {
                    push_param(obj);
                }
                ip += 1 + sizeof(obj);
                DISPATCH_NEXT;
            }

            DISPATCH(pop)
            {
                PREDICTED(pop);
                pop_param();
                ip += 1;
                PREDICT(get_local);
                DISPATCH_NEXT;
            }

            DISPATCH(push_value)
            {
                PREDICTED(push_value);
                auto val = *reinterpret_cast<const Value*>(ip+1);
                push_param(val);
                ip += 1 + sizeof(val);
                PREDICT(funcall);
                DISPATCH_NEXT;
            }

            DISPATCH(push_nil)
            {
                push_param(Value::nil());
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(push_fixnum_0)
            {
                push_param(Value::wrap_fixnum(0));
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(push_fixnum_1)
            {
                push_param(Value::wrap_fixnum(1));
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(instantiate_closure)
            {
                auto function = *reinterpret_cast<const Function* const*>(ip+1);
                auto instance = gc.alloc_object<Closure>(function);
                push_param(instance); // push this first so GC won't free it from under us
                auto closure = instance.as_object()->closure();
                if (function->has_captures())
                {

                    auto const &cap_offsets = function->capture_offsets();
                    for (size_t i = 0; i < cap_offsets.size(); ++i)
                    {
                        auto const &offs = cap_offsets[i];
                        Closure_Reference *ref;
                        if (offs.is_local)
                        {
                            ref = capture_closure_reference(m_locals + offs.index);
                        }
                        else
                        {
                            ref = m_current_closure.as_object()->closure()->get_reference(offs.index);
                        }
                        //printf("[%s] %d %p %s\n",
                        //       offs.name.c_str(),
                        //       offs.index,
                        //       ref->location(),
                        //       (ref->location() ? repr(ref->value()).c_str() : "#<nullptr>"));
                        closure->capture_reference(i, ref);
                    }
                }
                ip += 1 + sizeof(function);
                DISPATCH_NEXT;
            }

            DISPATCH(close_values)
            {
                auto n = *reinterpret_cast<const uint32_t*>(ip+1);

                close_values(m_stack_top-n);

                ip += 1 + sizeof(n);
                DISPATCH_NEXT;
            }

            DISPATCH(cons)
            {
                // Instead of popping twice, we use param_top(n) like this to serve as
                // a GC guard because before the GC may trigger a run before the cons
                // is allocated which may cause these values to be collected.
                auto val = gc.cons(param_top(-1), param_top(0));
                // THEN we pop the values after the cons is allocated.
                pop_params(2);
                push_param(val);
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(car)
            {
                auto o = pop_param();
                if (o.is_nil())
                {
                    push_param(o);
                }
                else
                {
                    CHECK_CONS(o);
                    push_param(car(o));
                }
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(cdr)
            {
                auto o = pop_param();
                if (o.is_nil())
                {
                    push_param(o);
                }
                else
                {
                    CHECK_CONS(o);
                    push_param(cdr(o));
                }
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(halt)
            {
                goto done;
            }

            DISPATCH(push_handler_case)
            {
                {
                    auto how_many = *reinterpret_cast<const uint32_t*>(ip+1);
                    auto branch = *reinterpret_cast<const uint32_t*>(ip+1+sizeof(how_many));
                    std::vector<Signal_Handler> handlers;
                    for (uint32_t i = 0; i < how_many; ++i)
                    {
                        auto tag = pop_param();
                        auto handler = pop_param();
                        handlers.push_back({tag, handler});
                    }
                    push_frame(ip + branch, 0);
                    push_handler_case(std::move(handlers));
                    ip += 1 + sizeof(how_many) + sizeof(branch);
                }
                DISPATCH_NEXT;
            }

            DISPATCH(raise_signal)
            {
                {
                    GC_GUARD();
                    // @Design, should we move the tag to be the first thing pushed since this just gets
                    // turned into a FUNCALL? The only reason to have the tag here is for easy access.
                    auto tag = pop_param();
                    auto nargs = *reinterpret_cast<const uint32_t*>(ip+1);
                    signal_args = to_list(m_stack_top - nargs, nargs);
                    signal_args = gc.cons(tag, signal_args);
                    GC_UNGUARD();
                }
                raise_signal:
                Handler_Case restore;
                Signal_Handler handler;
                if (find_handler(first(signal_args), true, restore, handler))
                {
                    m_stack_top = restore.stack;
                    m_call_frame_top = restore.frame;
                    // By default signal_args includes the handler tag and a specific handler knows its
                    // own tag because it is labeled as such. In the case of a handler with the T tag it
                    // is unknown so we leave it, otherwise it is removed.
                    auto ctx = alloc_signal_context(signal_args, ip);
                    if (handler.tag != g.s_T)
                    {
                        signal_args = cdr(signal_args);
                    }
                    func = handler.handler;
                    nargs = 1;
                    push_param(ctx);
                    while (!signal_args.is_nil())
                    {
                        ++nargs;
                        push_param(car(signal_args));
                        signal_args = cdr(signal_args);
                    }
                    call_type = Call_Type::Doesnt_Push_Frame;
                    with_return = false;
                    goto do_funcall;
                }
                else
                {
                    Signal_Context *ctx = nullptr;
                    if (first(signal_args).is_type(Object_Type::Signal_Context))
                    {
                        ctx = first(signal_args).as_object()->signal_context();
                    }
                    auto top = m_call_frame_top;
                    auto bottom = m_call_frame_bottom;
                    m_call_frame_top = m_call_frame_bottom;
                    m_stack_top = m_stack_bottom;
                    throw Signal_Exception(signal_args, ip, top, bottom, ctx);
                }
            }

            DISPATCH(eq)
            {
                auto b = pop_param();
                auto a = pop_param();
                if (a == b)
                {
                    push_param(g.s_T);
                }
                else if (a.is_type(Object_Type::System_Pointer) &&
                         b.is_type(Object_Type::System_Pointer) &&
                         (a.as_object()->system_pointer() == b.as_object()->system_pointer()))
                {
                    push_param(g.s_T);
                }
                else
                {
                    push_param(Value::nil());
                }
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(rplaca)
            {
                auto b = pop_param();
                auto a = param_top();
                CHECK_CONS(a);
                set_car(a, b);
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(rplacd)
            {
                auto b = pop_param();
                auto a = param_top();
                CHECK_CONS(a);
                set_cdr(a, b);
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(aref)
            {
                auto subscript = pop_param();
                CHECK_FIXNUM(subscript);
                auto array_val = pop_param();
                CHECK_SIMPLE_ARRAY(array_val);
                auto array = array_val.as_object()->simple_array();
                auto index = subscript.as_fixnum();
                if (index < 0 || index >= array->size())
                {
                    signal_args = gc.list(g.s_INDEX_OUT_OF_BOUNDS_ERROR, subscript, array_val);
                    goto raise_signal;
                }
                push_param(array->at(index));
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(aset)
            {
                auto value = pop_param();
                auto subscript = pop_param();
                CHECK_FIXNUM(subscript);
                auto array_val = pop_param();
                CHECK_SIMPLE_ARRAY(array_val);
                auto array = array_val.as_object()->simple_array();
                auto index = subscript.as_fixnum();
                if (index < 0 || index >= array->size())
                {
                    signal_args = gc.list(g.s_INDEX_OUT_OF_BOUNDS_ERROR, subscript, array_val);
                    goto raise_signal;
                }
                auto type = array->element_type();
                if (type != g.s_T)
                {
                    if (type == g.s_FIXNUM && !value.is_fixnum())
                    {
                        CHECK_FIXNUM(value);
                    }
                    else if (type == g.s_CHARACTER && !value.is_character())
                    {
                        CHECK_CHARACTER(value);
                    }
                }
                array->at(index) = value;
                push_param(value);
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(debug_trap)
            {
                g.debugger.breaking = !param_top().is_nil();
                if (g.debugger.breaking)
                {
                    g.debugger.command = Runtime_Globals::Debugger::Command::Step_Into;
                }
                else
                {
                    g.debugger.command = Runtime_Globals::Debugger::Command::Continue;
                }
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(add)
            {
                /*
                  This is a type deduction algorithm with minimal branching
                  // good types
                  int, int = 3
                  flo, int = 6
                  int, flo = 9
                  flo, flo = 12

                  // fail types
                  no, no = 0
                  int, no = 1
                  flo, no = 4
                  no, int = 2
                  no, flo = 8
                 */
                auto b = pop_param();
                auto a = pop_param();

                char a_t = 0;
                a_t += a.is_fixnum();
                a_t += a.is_type(Object_Type::Float) * 4;

                char b_t = 0;
                b_t += b.is_fixnum() * 2;
                b_t += b.is_type(Object_Type::Float) * 8;

                char c_t = a_t + b_t;

                if (c_t == 3)
                {
                    push_param(a + b);
                }
                else if (c_t == 6)
                {
                    Float f = b.as_fixnum();
                    f += a.as_object()->to_float();
                    push_param(gc.alloc_object<Float>(f));
                }
                else if (c_t == 9)
                {
                    Float f = a.as_fixnum();
                    f += b.as_object()->to_float();
                    push_param(gc.alloc_object<Float>(f));
                }
                else if (c_t == 12)
                {
                    Float f = a.as_object()->to_float();
                    f += b.as_object()->to_float();
                    push_param(gc.alloc_object<Float>(f));
                }
                else
                {
                    GC_GUARD();
                    signal_args = gc.list(g.s_TYPE_ERROR, gc.list(g.s_OR, g.s_FIXNUM, g.s_FLOAT), a, b);
                    GC_UNGUARD();
                    goto raise_signal;
                }
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(add_1)
            {
                if (param_top().is_fixnum())
                {
                    param_top() += Value::wrap_fixnum(1);
                }
                else if (param_top().is_type(Object_Type::Float))
                {
                    Float f = pop_param().as_object()->to_float();
                    push_param(gc.alloc_object<Float>(f + 1.0));
                }
                ip += 1;
                PREDICT(set_local);
                DISPATCH_NEXT;
            }

            DISPATCH(sub)
            {
                /*
                  This is a type deduction algorithm with minimal branching
                  // good types
                  int, int = 3
                  flo, int = 6
                  int, flo = 9
                  flo, flo = 12

                  // fail types
                  no, no = 0
                  int, no = 1
                  flo, no = 4
                  no, int = 2
                  no, flo = 8
                 */
                auto b = pop_param();
                auto a = pop_param();

                char a_t = 0;
                a_t += a.is_fixnum();
                a_t += a.is_type(Object_Type::Float) * 4;

                char b_t = 0;
                b_t += b.is_fixnum() * 2;
                b_t += b.is_type(Object_Type::Float) * 8;

                char c_t = a_t + b_t;

                if (c_t == 3)
                {
                    push_param(a - b);
                }
                else if (c_t == 6)
                {
                    Float f = b.as_fixnum();
                    f -= a.as_object()->to_float();
                    push_param(gc.alloc_object<Float>(f));
                }
                else if (c_t == 9)
                {
                    Float f = a.as_fixnum();
                    f -= b.as_object()->to_float();
                    push_param(gc.alloc_object<Float>(f));
                }
                else if (c_t == 12)
                {
                    Float f = a.as_object()->to_float();
                    f -= b.as_object()->to_float();
                    push_param(gc.alloc_object<Float>(f));
                }
                else
                {
                    GC_GUARD();
                    signal_args = gc.list(g.s_TYPE_ERROR, gc.list(g.s_OR, g.s_FIXNUM, g.s_FLOAT), a, b);
                    GC_UNGUARD();
                    goto raise_signal;
                }
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(sub_1)
            {
                if (param_top().is_fixnum())
                {
                    param_top() -= Value::wrap_fixnum(1);
                }
                else if (param_top().is_type(Object_Type::Float))
                {
                    Float f = pop_param().as_object()->to_float();
                    push_param(gc.alloc_object<Float>(f - 1.0));
                }
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(dup)
            {
                push_param(param_top());
                ip += 1;
                DISPATCH_NEXT;
            }
        }
    }
    done:

    return ip;
}

const uint8_t *VM_State::execute_impl_debug(const uint8_t *ip)
{
    return execute_impl<true>(ip);
}

const uint8_t *VM_State::execute_impl_nodebug(const uint8_t *ip)
{
    return execute_impl<false>(ip);
}
