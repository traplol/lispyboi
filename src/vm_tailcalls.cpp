#include <iomanip>

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
            TAILCALL(shared_do_raise_signal);                           \
        }                                                               \
    } while (0)

using namespace lisp;

enum class Call_Type
{
    Doesnt_Push_Frame,
    Pushes_Frame,
};

#define OPCODE(name) [[gnu::noinline]] static const uint8_t *execute_ ## name (const uint8_t *ip, VM_State &vm, Call_Type call_type, Value func, uint32_t nargs, Value signal_args)

#define DISPATCH_NEXT [[clang::musttail]]return execute_table[*ip](ip, vm, call_type, func, nargs, signal_args)
#define TAILCALL(name) [[clang::musttail]]return execute_ ## name(ip, vm, call_type, func, nargs, signal_args)
#define TAILCALLABLE(name) [[gnu::noinline]] static const uint8_t *execute_ ## name (const uint8_t *ip, VM_State &vm, Call_Type call_type, Value func, uint32_t nargs, Value signal_args)

// Forward Declarations
#define BYTECODE_DEF(name, noperands, nargs, size, docstring) OPCODE(name);
#include "bytecode.def"

TAILCALLABLE(shared_do_funcall);
TAILCALLABLE(shared_do_raise_signal);

#define BYTECODE_DEF(name, noperands, nargs, size, docstring) &execute_ ## name,
constexpr std::array execute_table =
    {
        #include "bytecode.def"
    };

#define PREDICT(name) // empty
#define PREDICTED(name) // empty

OPCODE(apply)
{
    func = vm.pop_param();
    nargs = *reinterpret_cast<const uint32_t*>(ip+1);
    if (nargs == 0)
    {
        GC_GUARD();
        signal_args = gc.list(g.s_SIMPLE_ERROR, gc.alloc_string("Too few arguments!"));
        GC_UNGUARD();
        TAILCALL(shared_do_raise_signal);
    }

    auto last_arg = vm.pop_param();
    CHECK_LIST(last_arg);
    nargs--;
    while (!last_arg.is_nil())
    {
        vm.push_param(car(last_arg));
        last_arg = cdr(last_arg);
        nargs++;
    }
    [[clang::musttail]] return execute_shared_do_funcall(ip, vm, Call_Type::Pushes_Frame, func, nargs, signal_args);
}

OPCODE(funcall)
{
    PREDICTED(funcall);
    func = vm.pop_param();
    nargs = *reinterpret_cast<const uint32_t*>(ip+1);
    [[clang::musttail]] return execute_shared_do_funcall(ip, vm, Call_Type::Pushes_Frame, func, nargs, signal_args);
}

OPCODE(gotocall)
{

    vm.close_values(vm.m_locals);

    func = vm.pop_param();
    nargs = *reinterpret_cast<const uint32_t*>(ip+1);
    auto begin = vm.m_stack_top - nargs;
    auto end = vm.m_stack_top;
    vm.m_stack_top = vm.m_locals;
    std::copy(begin, end, vm.m_stack_top);
    vm.m_stack_top += nargs;
    [[clang::musttail]] return execute_shared_do_funcall(ip, vm, Call_Type::Doesnt_Push_Frame, func, nargs, signal_args);
}

TAILCALLABLE(funcall_too_few_args)
{
    auto closure = func.as_object()->closure();
    auto function = closure->function();
    GC_GUARD();
    signal_args = gc.list(g.s_SIMPLE_ERROR,
                          gc.alloc_string("Too few arguments!"),
                          func,
                          Value::wrap_fixnum(function->arity()),
                          Value::wrap_fixnum(nargs));
    GC_UNGUARD();
    TAILCALL(shared_do_raise_signal);
}

TAILCALLABLE(funcall_too_many_args)
{
    auto closure = func.as_object()->closure();
    auto function = closure->function();
    GC_GUARD();
    signal_args = gc.list(g.s_SIMPLE_ERROR,
                          gc.alloc_string("Too many arguments!"),
                          func,
                          Value::wrap_fixnum(function->arity()),
                          Value::wrap_fixnum(nargs));
    GC_UNGUARD();
    TAILCALL(shared_do_raise_signal);
}

TAILCALLABLE(shared_do_funcall)
{
    auto ofunc = func;
    if (func.is_type(Object_Type::Closure))
        goto skip_lookup;

    if (symbolp(func))
    {
        func = func.as_object()->symbol()->function();
    }

    if (func.is_type(Object_Type::Closure))
    {
        skip_lookup:
        // Although op_raise_signal also jumps here in a "funcall"-like way, it has
        // already setup the stack in the state the closure expects to execute under
        // so there is no need to push a frame for it here.
        if (call_type == Call_Type::Pushes_Frame)
        {
            vm.push_frame(ip+5, nargs);
        }

        vm.m_current_closure = func;
        auto closure = func.as_object()->closure();
        auto function = closure->function();

        // locals always start at first argument
        vm.m_locals = vm.m_stack_top - nargs;

        if (function->is_too_many_args(nargs))
            TAILCALL(funcall_too_many_args);

        if (function->is_too_few_args(nargs))
            TAILCALL(funcall_too_few_args);

        if (function->has_rest() && nargs > function->rest_index())
        {
            auto rest = to_list(vm.m_locals+function->rest_index(),
                                nargs - function->rest_index());
            vm.m_locals[function->rest_index()] = rest;
        }

        vm.m_stack_top = vm.m_locals + function->num_locals();
#if DEBUG > 1
        {
            assert(function->arity() <= function->num_locals());
            auto start = vm.m_locals + function->arity();
            auto end = vm.m_locals + function->num_locals();
            for (; start != end; ++start)
            {
                *start = Value::nil();
            }
        }
#endif

        ip = function->entrypoint(nargs);
        DISPATCH_NEXT;
    }


    if (func.is_lisp_primitive())
    {
        bool raised_signal = false;
        auto primitive = func.as_lisp_primitive();
        auto result = primitive(vm.m_stack_top - nargs, nargs, raised_signal);
        vm.m_stack_top -= nargs;
        if (raised_signal)
        {
            signal_args = result;
            TAILCALL(shared_do_raise_signal);
        }
        else
        {
            vm.push_param(result);
            ip += 1 + sizeof(nargs);
        }
        DISPATCH_NEXT;
    }

    // error
    GC_GUARD();
    signal_args = gc.list(g.s_SIMPLE_ERROR, gc.alloc_string("Not a callable object"), ofunc, func);
    GC_UNGUARD();
    TAILCALL(shared_do_raise_signal);
}

OPCODE(raise_signal)
{
        GC_GUARD();
        // @Design, should we move the tag to be the first thing pushed since this just gets
        // turned into a FUNCALL? The only reason to have the tag here is for easy access.
        auto tag_ = vm.pop_param();
        auto nargs_ = *reinterpret_cast<const uint32_t*>(ip+1);
        signal_args = to_list(vm.m_stack_top - nargs_, nargs_);
        signal_args = gc.cons(tag_, signal_args);
        GC_UNGUARD();

        TAILCALL(shared_do_raise_signal);
}

TAILCALLABLE(shared_do_raise_signal)
{
    {
        VM_State::Handler_Case restore;
        VM_State::Signal_Handler handler;
        if (vm.find_handler(first(signal_args), true, restore, handler))
        {
            vm.m_stack_top = restore.stack;
            vm.m_call_frame_top = restore.frame;
            // By default signal_args includes the handler tag and a specific handler knows its
            // own tag because it is labeled as such. In the case of a handler with the T tag it
            // is unknown so we leave it, otherwise it is removed.
            auto ctx = vm.alloc_signal_context(signal_args, ip);
            if (handler.tag != g.s_T)
            {
                signal_args = cdr(signal_args);
            }
            func = handler.handler;
            nargs = 1;
            vm.push_param(ctx);
            while (!signal_args.is_nil())
            {
                ++nargs;
                vm.push_param(car(signal_args));
                signal_args = cdr(signal_args);
            }
        }
        else
        {
            Signal_Context *ctx = nullptr;
            if (first(signal_args).is_type(Object_Type::Signal_Context))
            {
                ctx = first(signal_args).as_object()->signal_context();
            }
            auto top = vm.m_call_frame_top;
            auto bottom = vm.m_call_frame_bottom;
            vm.m_call_frame_top = vm.m_call_frame_bottom;
            vm.m_stack_top = vm.m_stack_bottom;
            throw VM_State::Signal_Exception(signal_args, ip, top, bottom, ctx);
        }
    }
    [[clang::musttail]]return execute_shared_do_funcall(ip, vm, Call_Type::Doesnt_Push_Frame, func, nargs, signal_args);
}

OPCODE(pop_handler_case)
{
    vm.pop_handler_case();
    TAILCALL(return);
}

OPCODE(return)
{
    if (vm.m_call_frame_top == vm.m_call_frame_bottom)
    {
        return ip;
    }

    vm.close_values(vm.m_locals);

    auto val = vm.param_top();
    auto frame = vm.pop_frame();
    vm.set_frame(frame);
    vm.push_param(val);
    if (frame.ip == nullptr)
    {
        return ip;
    }
    ip = frame.ip;
    DISPATCH_NEXT;
}

OPCODE(jump)
{
    auto offset = *reinterpret_cast<const int32_t*>(ip+1);
    ip += offset;
    DISPATCH_NEXT;
}

OPCODE(pop_jump_if_nil)
{
    auto jump_mask = ~(static_cast<uintptr_t>(vm.pop_param().is_nil()) - 1);
    auto offset = *reinterpret_cast<const int32_t*>(ip+1);
    ip += (offset & jump_mask) | (5 & ~jump_mask);
    //if (vm.pop_param().is_nil())
    //{
    //    auto offset = *reinterpret_cast<const int32_t*>(ip+1);
    //    ip += offset;
    //}
    //else
    //{
    //    ip += 5;
    //}
    DISPATCH_NEXT;
}

OPCODE(get_global)
{
    auto index = *reinterpret_cast<const uint32_t*>(ip+1);
    vm.push_param(g.global_value_slots[index]);
    ip += 5;
    DISPATCH_NEXT;
}

OPCODE(set_global)
{
    auto index = *reinterpret_cast<const uint32_t*>(ip+1);
    g.global_value_slots[index] = vm.param_top();
    ip += 5;
    PREDICT(pop);
    DISPATCH_NEXT;
}

OPCODE(get_local)
{
    PREDICTED(get_local);
    auto index = *reinterpret_cast<const uint32_t*>(ip+1);
    vm.push_param(vm.m_locals[index]);
    ip += 5;
    PREDICT(push_value);
    PREDICT(get_local);
    DISPATCH_NEXT;
}

OPCODE(set_local)
{
    PREDICTED(set_local)
        auto index = *reinterpret_cast<const uint32_t*>(ip+1);
    vm.m_locals[index] = vm.param_top();
    ip += 5;
    PREDICT(pop);
    DISPATCH_NEXT;
}

OPCODE(get_capture)
{
    auto index = *reinterpret_cast<const uint32_t*>(ip+1);
    vm.push_param(vm.m_current_closure.as_object()->closure()->get_capture(index));
    ip += 5;
    DISPATCH_NEXT;
}

OPCODE(set_capture)
{
    auto index = *reinterpret_cast<const uint32_t*>(ip+1);
    vm.m_current_closure.as_object()->closure()->set_capture(index, vm.param_top());
    ip += 5;
    PREDICT(pop);
    DISPATCH_NEXT;
}

OPCODE(function_value)
{
    auto obj = *reinterpret_cast<const Value*>(ip+1);
    if (symbolp(obj))
    {
        vm.push_param(obj.as_object()->symbol()->function());
    }
    else
    {
        vm.push_param(obj);
    }
    ip += 1 + sizeof(obj);
    DISPATCH_NEXT;
}

OPCODE(pop)
{
    PREDICTED(pop);
    vm.pop_param();
    ip += 1;
    PREDICT(get_local);
    DISPATCH_NEXT;
}

OPCODE(push_value)
{
    PREDICTED(push_value);
    auto val = *reinterpret_cast<const Value*>(ip+1);
    vm.push_param(val);
    ip += 1 + sizeof(val);
    PREDICT(funcall);
    DISPATCH_NEXT;
}

OPCODE(push_nil)
{
    vm.push_param(Value::nil());
    ip += 1;
    DISPATCH_NEXT;
}

OPCODE(push_fixnum_0)
{
    vm.push_param(Value::wrap_fixnum(0));
    ip += 1;
    DISPATCH_NEXT;
}

OPCODE(push_fixnum_1)
{
    vm.push_param(Value::wrap_fixnum(1));
    ip += 1;
    DISPATCH_NEXT;
}

OPCODE(instantiate_closure)
{
    auto function = *reinterpret_cast<const Function* const*>(ip+1);
    auto instance = gc.alloc_object<Closure>(function);
    vm.push_param(instance); // push this first so GC won't free it from under us
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
                ref = vm.capture_closure_reference(vm.m_locals + offs.index);
            }
            else
            {
                ref = vm.m_current_closure.as_object()->closure()->get_reference(offs.index);
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

OPCODE(close_values)
{
    auto n = *reinterpret_cast<const uint32_t*>(ip+1);

    vm.close_values(vm.m_stack_top-n);

    ip += 1 + sizeof(n);
    DISPATCH_NEXT;
}

OPCODE(cons)
{
    // Instead of popping twice, we use param_top(n) like this to serve as
    // a GC guard because before the GC may trigger a run before the cons
    // is allocated which may cause these values to be collected.
    auto val = gc.cons(vm.param_top(-1), vm.param_top(0));
    // THEN we pop the values after the cons is allocated.
    vm.pop_params(2);
    vm.push_param(val);
    ip += 1;
    DISPATCH_NEXT;
}

OPCODE(car)
{
    auto o = vm.pop_param();
    if (o.is_nil())
    {
        vm.push_param(o);
    }
    else
    {
        CHECK_CONS(o);
        vm.push_param(car(o));
    }
    ip += 1;
    DISPATCH_NEXT;
}

OPCODE(cdr)
{
    auto o = vm.pop_param();
    if (o.is_nil())
    {
        vm.push_param(o);
    }
    else
    {
        CHECK_CONS(o);
        vm.push_param(cdr(o));
    }
    ip += 1;
    DISPATCH_NEXT;
}

OPCODE(halt)
{
    return ip;
}

OPCODE(push_handler_case)
{
    {
        auto how_many = *reinterpret_cast<const uint32_t*>(ip+1);
        auto branch = *reinterpret_cast<const uint32_t*>(ip+1+sizeof(how_many));
        std::vector<VM_State::Signal_Handler> handlers;
        for (uint32_t i = 0; i < how_many; ++i)
        {
            auto tag = vm.pop_param();
            auto handler = vm.pop_param();
            handlers.push_back({tag, handler});
        }
        vm.push_frame(ip + branch, 0);
        vm.push_handler_case(std::move(handlers));
        ip += 1 + sizeof(how_many) + sizeof(branch);
    }
    DISPATCH_NEXT;
}

OPCODE(eq)
{
    auto b = vm.pop_param();
    auto a = vm.pop_param();
    if (a == b)
    {
        vm.push_param(g.s_T);
    }
    else if (a.is_type(Object_Type::System_Pointer) &&
             b.is_type(Object_Type::System_Pointer) &&
             (a.as_object()->system_pointer() == b.as_object()->system_pointer()))
    {
        vm.push_param(g.s_T);
    }
    else
    {
        vm.push_param(Value::nil());
    }
    ip += 1;
    DISPATCH_NEXT;
}

OPCODE(rplaca)
{
    auto b = vm.pop_param();
    auto a = vm.param_top();
    CHECK_CONS(a);
    set_car(a, b);
    ip += 1;
    DISPATCH_NEXT;
}

OPCODE(rplacd)
{
    auto b = vm.pop_param();
    auto a = vm.param_top();
    CHECK_CONS(a);
    set_cdr(a, b);
    ip += 1;
    DISPATCH_NEXT;
}

OPCODE(aref)
{
    auto subscript = vm.pop_param();
    CHECK_FIXNUM(subscript);
    auto array_val = vm.pop_param();
    CHECK_SIMPLE_ARRAY(array_val);
    auto array = array_val.as_object()->simple_array();
    auto index = subscript.as_fixnum();
    if (index < 0 || index >= array->size())
    {
        signal_args = gc.list(g.s_INDEX_OUT_OF_BOUNDS_ERROR, subscript, array_val);
        TAILCALL(shared_do_raise_signal);
    }
    vm.push_param(array->at(index));
    ip += 1;
    DISPATCH_NEXT;
}

OPCODE(aset)
{
    auto value = vm.pop_param();
    auto subscript = vm.pop_param();
    CHECK_FIXNUM(subscript);
    auto array_val = vm.pop_param();
    CHECK_SIMPLE_ARRAY(array_val);
    auto array = array_val.as_object()->simple_array();
    auto index = subscript.as_fixnum();
    if (index < 0 || index >= array->size())
    {
        signal_args = gc.list(g.s_INDEX_OUT_OF_BOUNDS_ERROR, subscript, array_val);
        TAILCALL(shared_do_raise_signal);
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
    vm.push_param(value);
    ip += 1;
    DISPATCH_NEXT;
}

OPCODE(debug_trap)
{
    //g.debugger.breaking = !param_top().is_nil();
    //if (g.debugger.breaking)
    //{
    //    g.debugger.command = Runtime_Globals::Debugger::Command::Step_Into;
    //}
    //else
    //{
    //    g.debugger.command = Runtime_Globals::Debugger::Command::Continue;
    //}
    ip += 1;
    DISPATCH_NEXT;
}

OPCODE(add)
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
    auto b = vm.pop_param();
    auto a = vm.pop_param();

    char a_t = 0;
    a_t += a.is_fixnum();
    a_t += a.is_type(Object_Type::Float) * 4;

    char b_t = 0;
    b_t += b.is_fixnum() * 2;
    b_t += b.is_type(Object_Type::Float) * 8;

    char c_t = a_t + b_t;

    if (c_t == 3)
    {
        vm.push_param(a + b);
    }
    else if (c_t == 6)
    {
        Float f = b.as_fixnum();
        f += a.as_object()->to_float();
        vm.push_param(gc.alloc_object<Float>(f));
    }
    else if (c_t == 9)
    {
        Float f = a.as_fixnum();
        f += b.as_object()->to_float();
        vm.push_param(gc.alloc_object<Float>(f));
    }
    else if (c_t == 12)
    {
        Float f = a.as_object()->to_float();
        f += b.as_object()->to_float();
        vm.push_param(gc.alloc_object<Float>(f));
    }
    else
    {
        GC_GUARD();
        signal_args = gc.list(g.s_TYPE_ERROR, gc.list(g.s_OR, g.s_FIXNUM, g.s_FLOAT), a, b);
        GC_UNGUARD();
        TAILCALL(shared_do_raise_signal);
    }
    ip += 1;
    DISPATCH_NEXT;
}

OPCODE(add_1)
{
    if (vm.param_top().is_fixnum())
    {
        vm.param_top() += Value::wrap_fixnum(1);
    }
    else if (vm.param_top().is_type(Object_Type::Float))
    {
        Float f = vm.pop_param().as_object()->to_float();
        vm.push_param(gc.alloc_object<Float>(f + 1.0));
    }
    ip += 1;
    PREDICT(set_local);
    DISPATCH_NEXT;
}

OPCODE(sub)
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
    auto b = vm.pop_param();
    auto a = vm.pop_param();

    char a_t = 0;
    a_t += a.is_fixnum();
    a_t += a.is_type(Object_Type::Float) * 4;

    char b_t = 0;
    b_t += b.is_fixnum() * 2;
    b_t += b.is_type(Object_Type::Float) * 8;

    char c_t = a_t + b_t;

    if (c_t == 3)
    {
        vm.push_param(a - b);
    }
    else if (c_t == 6)
    {
        Float f = b.as_fixnum();
        f -= a.as_object()->to_float();
        vm.push_param(gc.alloc_object<Float>(f));
    }
    else if (c_t == 9)
    {
        Float f = a.as_fixnum();
        f -= b.as_object()->to_float();
        vm.push_param(gc.alloc_object<Float>(f));
    }
    else if (c_t == 12)
    {
        Float f = a.as_object()->to_float();
        f -= b.as_object()->to_float();
        vm.push_param(gc.alloc_object<Float>(f));
    }
    else
    {
        GC_GUARD();
        signal_args = gc.list(g.s_TYPE_ERROR, gc.list(g.s_OR, g.s_FIXNUM, g.s_FLOAT), a, b);
        GC_UNGUARD();
        TAILCALL(shared_do_raise_signal);
    }
    ip += 1;
    DISPATCH_NEXT;
}

OPCODE(sub_1)
{
    if (vm.param_top().is_fixnum())
    {
        vm.param_top() -= Value::wrap_fixnum(1);
    }
    else if (vm.param_top().is_type(Object_Type::Float))
    {
        Float f = vm.pop_param().as_object()->to_float();
        vm.push_param(gc.alloc_object<Float>(f - 1.0));
    }
    ip += 1;
    DISPATCH_NEXT;
}

const uint8_t *VM_State::execute_impl_tailcalls(const uint8_t *ip)
{
    return execute_table[*ip](ip, *this, Call_Type::Doesnt_Push_Frame, Value::nil(), 0, Value::nil());
}


//#define BYTECODE_DEF(name, noperands, nargs, size, docstring) &&opcode_ ## name,
//    void *computed_gotos[256] =
//    {
//        #include "bytecode.def"
//    };
//
//#define DISPATCH(name) case bytecode::Opcode::op_ ## name: opcode_ ## name:
//
//#define DISPATCH_NEXT goto *computed_gotos[*ip];
//#define EXEC switch(static_cast<bytecode::Opcode>(*ip))
//#define DISPATCH_LOOP for (;;)
//
//#if PROFILE_OPCODE_PAIRS
//#define PREDICTED(name) //empty
//#define PREDICT(name) //empty
//#else
//#define PREDICTED(name) predicted_ ## name:
//#define PREDICT(name)                                                 \
//    do {                                                                \
//        if (static_cast<bytecode::Opcode>(*ip) == bytecode::Opcode::op_ ## name) \
//        {                                                               \
//            goto predicted_ ## name;                                       \
//        }                                                               \
//    } while (0)
//#endif
//
//    static_assert(sizeof(*ip) == 1, "pointer arithmetic will not work as expected.");
//    Value signal_args;
//    Value func;
//    uint32_t nargs;
//    enum class Call_Type
//    {
//        Doesnt_Push_Frame,
//        Pushes_Frame,
//    } call_type;
//    DISPATCH_LOOP
//    {
//        EXEC
//        {
//        }
//    }
//    done:
