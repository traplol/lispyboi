#include "util.hpp"
#include "bytecode/opcode.hpp"
#include "bytecode/emitter.hpp"
#include "bytecode/compiler.hpp"
#include "bytecode/disassemble.hpp"
#include "vm_state.hpp"

#define TYPE_CHECK(what, typecheck, expected)                           \
    do {                                                                \
        if (!(what).typecheck) {                                        \
            aux1 = what;                                                \
            aux2 = expected;                                            \
            TAILCALL(type_check_failed);                                \
        }                                                               \
    } while (0)

using namespace lisp;

enum Call_Type
{
    Doesnt_Push_Frame,
    Pushes_Frame,
};

/*
 * The decision to use Value::Bits_Type aux1, aux2, aux3, and aux4 is an x64 optimization
 * where the System V ABI calling convention requires the first 6 integer or pointer arguments
 * to be passed via registers. And since C++ doesn't directly let use tell the compiler which
 * variables to keep in registers we will just exploit the ABI requirements. :)
 *
 * This can create some hairy looking code where we try to minimize usage of locals and do
 * everything with our 4 auxiliary registers but it can be a significant boost to performance
 *
 * A general rule of thumb to take advantage of this optimization is to hoist code that would
 * allocate on the stack or instantiate things with non-trivial destructors, such as std::string
 * or std::vector, into their own TAILCALLABLE function definition and TAILCALL there.
 */

#define OPCODE(name) [[gnu::noinline]] static const uint8_t *execute_ ## name (void *execute_table, const uint8_t *ip, VM_State &vm, Value::Bits_Type aux1, Value::Bits_Type aux2, Value::Bits_Type aux3)
using Exec_Function = const uint8_t *(*)(void*, const uint8_t*, VM_State&, Value::Bits_Type, Value::Bits_Type, Value::Bits_Type);
//#define call_type aux1
#define func aux1
#define nargs aux2
#define signal_args aux3

#define DISPATCH_NEXT [[clang::musttail]]return reinterpret_cast<Exec_Function*>(execute_table)[*ip](execute_table, ip, vm, aux1, aux2, aux3)
#define TAILCALL(name) [[clang::musttail]]return execute_ ## name(execute_table, ip, vm, aux1, aux2, aux3)
//#define TAILCALLABLE(name) [[gnu::noinline]] static const uint8_t *execute_ ## name (void *execute_table, const uint8_t *ip, VM_State &vm, Value::Bits_Type aux1, Value::Bits_Type aux2, Value::Bits_Type aux3)
#define TAILCALLABLE(name) OPCODE(name)

#define PREDICT(name)                                                 \
    do {                                                                \
        if (static_cast<bytecode::Opcode>(*ip) == bytecode::Opcode::op_ ## name) \
        {                                                               \
            TAILCALL(name);                                             \
        }                                                               \
    } while (0)
//#define PREDICT(name) // empty

// Forward Declarations
#define BYTECODE_DEF(name, noperands, nargs, size, docstring) OPCODE(name);
#include "bytecode.def"

template<bool pushes_frame>
TAILCALLABLE(shared_do_funcall);
TAILCALLABLE(shared_do_raise_signal);

#define BYTECODE_DEF(name, noperands, nargs, size, docstring) &execute_ ## name,
constexpr std::array execute_table =
    {
        #include "bytecode.def"
    };

TAILCALLABLE(type_check_failed)
{
    signal_args = gc.list(g.s_TYPE_ERROR, Value(aux2), Value(aux1));
    TAILCALL(shared_do_raise_signal);
}

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
    [[clang::musttail]] return execute_shared_do_funcall<true>(execute_table, ip, vm, aux1, aux2, aux3);
}

TAILCALLABLE(gotocall_call_primitive)
{
    bool raised_signal = false;
    auto primitive = Value(func).as_lisp_primitive();
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
        ip += 5;
        TAILCALL(return);
    }
}

OPCODE(gotocall)
{

    vm.close_values(vm.m_locals);

    func = vm.pop_param();
    nargs = *reinterpret_cast<const uint32_t*>(ip+1);

    if (symbolp(Value(func)))
    {
        func = Value(func).as_object()->symbol()->function();
    }

    auto begin = vm.m_stack_top - nargs;
    auto end = vm.m_stack_top;
    vm.m_stack_top = vm.m_locals;
    std::copy(begin, end, vm.m_stack_top);
    vm.m_stack_top += nargs;
    if (Value(func).is_lisp_primitive())
    {
        TAILCALL(gotocall_call_primitive);
    }
    else
    {
        TAILCALL(shared_do_funcall<false>);
    }
}

TAILCALLABLE(funcall_too_few_args)
{
    auto closure = Value(func).as_object()->closure();
    auto function = closure->function();
    GC_GUARD();
    signal_args = gc.list(g.s_SIMPLE_ERROR,
                          gc.alloc_string("Too few arguments!"),
                          Value(func),
                          Value::wrap_fixnum(function->arity()),
                          Value::wrap_fixnum(nargs));
    GC_UNGUARD();
    TAILCALL(shared_do_raise_signal);
}

TAILCALLABLE(funcall_too_many_args)
{
    auto closure = Value(func).as_object()->closure();
    auto function = closure->function();
    GC_GUARD();
    signal_args = gc.list(g.s_SIMPLE_ERROR,
                          gc.alloc_string("Too many arguments!"),
                          Value(func),
                          Value::wrap_fixnum(function->arity()),
                          Value::wrap_fixnum(nargs));
    GC_UNGUARD();
    TAILCALL(shared_do_raise_signal);
}

TAILCALLABLE(funcall_not_callable_object)
{
    GC_GUARD();
    signal_args = gc.list(g.s_SIMPLE_ERROR,
                          gc.alloc_string("Not a callable object"),
                          Value(signal_args));
    GC_UNGUARD();
    TAILCALL(shared_do_raise_signal);
}

TAILCALLABLE(funcall_call_primitive)
{
    bool raised_signal = false;
    auto primitive = Value(func).as_lisp_primitive();
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
        ip += 5;
        DISPATCH_NEXT;
    }
}

TAILCALLABLE(funcall_with_rest_args)
{
    auto closure = Value(func).as_object()->closure();
    auto function = closure->function();
    {
        auto rest = to_list(vm.m_locals+function->rest_index(),
                            nargs - function->rest_index());
        vm.m_locals[function->rest_index()] = rest;
    }

    vm.m_stack_top = vm.m_locals + function->num_locals();
    ip = function->entrypoint(nargs);
    DISPATCH_NEXT;
}

TAILCALLABLE(funcall_error)
{
    // error
    GC_GUARD();
    signal_args = gc.list(g.s_SIMPLE_ERROR, gc.alloc_string("Not a callable object"), Value(signal_args), Value(func));
    GC_UNGUARD();
    TAILCALL(shared_do_raise_signal);
}

OPCODE(funcall)
{
    func = vm.pop_param();
    nargs = *reinterpret_cast<const uint32_t*>(ip+1);
    [[clang::musttail]] return execute_shared_do_funcall<true>(execute_table, ip, vm, aux1, aux2, aux3);
}

template<bool pushes_frame>
TAILCALLABLE(shared_do_funcall)
{
    signal_args = func;
    if (Value(func).is_type(Object_Type::Closure))
        goto skip_lookup;

    if (symbolp(Value(func)))
    {
        func = Value(func).as_object()->symbol()->function();
    }

    if (Value(func).is_type(Object_Type::Closure))
    {
        skip_lookup:
        // Although op_raise_signal also jumps here in a "funcall"-like way, it has
        // already setup the stack in the state the closure expects to execute under
        // so there is no need to push a frame for it here.
        if constexpr (pushes_frame)
        {
            vm.push_frame(ip+5, nargs);
        }

        vm.m_current_closure = Value(func);

        auto function = Value(func).as_object()->closure()->function();

        // locals always start at first argument
        vm.m_locals = vm.m_stack_top - nargs;

        if (function->has_rest() && nargs > function->rest_index())
            TAILCALL(funcall_with_rest_args);

        if (function->is_too_many_args(nargs))
            TAILCALL(funcall_too_many_args);

        if (function->is_too_few_args(nargs))
            TAILCALL(funcall_too_few_args);

        vm.m_stack_top = vm.m_locals + function->num_locals();

        ip = function->entrypoint(nargs);
        DISPATCH_NEXT;
    }


    if (Value(func).is_lisp_primitive())
    {
        TAILCALL(funcall_call_primitive);
    }

    TAILCALL(funcall_not_callable_object);
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
    aux1 = *reinterpret_cast<const int32_t*>(ip+1);
    ip += aux1;
    DISPATCH_NEXT;
}

OPCODE(pop_jump_if_nil)
{
    //auto jump_mask = ~(static_cast<uintptr_t>(vm.pop_param().is_nil()) - 1);
    //auto offset = *reinterpret_cast<const int32_t*>(ip+1);
    //ip += (offset & jump_mask) | (5 & ~jump_mask);
    if (vm.pop_param().is_nil())
    {
        aux1 = *reinterpret_cast<const int32_t*>(ip+1);
        ip += aux1;
    }
    else
    {
        ip += 5;
    }
    DISPATCH_NEXT;
}

OPCODE(get_global)
{
    aux1 = *reinterpret_cast<const uint32_t*>(ip+1);
    vm.push_param(g.global_value_slots[aux1]);
    ip += 5;
    DISPATCH_NEXT;
}

OPCODE(set_global)
{
    aux1 = *reinterpret_cast<const uint32_t*>(ip+1);
    g.global_value_slots[aux1] = vm.param_top();
    ip += 5;
    //PREDICT(pop);
    DISPATCH_NEXT;
}

OPCODE(get_local)
{
    aux1 = *reinterpret_cast<const uint32_t*>(ip+1);
    vm.push_param(vm.m_locals[aux1]);
    ip += 5;
    //PREDICT(get_local);
    DISPATCH_NEXT;
}

OPCODE(set_local)
{
    aux1 = *reinterpret_cast<const uint32_t*>(ip+1);
    vm.m_locals[aux1] = vm.param_top();
    ip += 5;
    //PREDICT(pop);
    DISPATCH_NEXT;
}

OPCODE(get_capture)
{
    aux1 = *reinterpret_cast<const uint32_t*>(ip+1);
    vm.push_param(vm.m_current_closure.as_object()->closure()->get_capture(aux1));
    ip += 5;
    DISPATCH_NEXT;
}

OPCODE(set_capture)
{
    aux1 = *reinterpret_cast<const uint32_t*>(ip+1);
    vm.m_current_closure.as_object()->closure()->set_capture(aux1, vm.param_top());
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
    vm.pop_param();
    ip += 1;
    DISPATCH_NEXT;
}

OPCODE(push_value)
{
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
    if (vm.param_top().is_nil())
    {
        // nothing
    }
    else
    {
        aux2 = vm.pop_param();
        CHECK_CONS(Value(aux2));
        vm.push_param(car(Value(aux2)));
    }
    ip += 1;
    DISPATCH_NEXT;
}

OPCODE(cdr)
{
    if (vm.param_top().is_nil())
    {
        // nothing
    }
    else
    {
        aux2 = vm.pop_param();
        CHECK_CONS(Value(aux2));
        vm.push_param(cdr(Value(aux2)));
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
    aux2 = vm.pop_param();
    aux1 = vm.pop_param();
    if (aux1 == aux2)
    {
        vm.push_param(g.s_T);
        ip += 1;
        DISPATCH_NEXT;
    }
    if (Value(aux1).is_type(Object_Type::System_Pointer) &&
        Value(aux2).is_type(Object_Type::System_Pointer) &&
        Value(aux1).as_object()->system_pointer() ==
        Value(aux2).as_object()->system_pointer())
    {
        vm.push_param(g.s_T);
        ip += 1;
        DISPATCH_NEXT;
    }
    vm.push_param(Value::nil());
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

TAILCALLABLE(add_sub_type_error)
{
    GC_GUARD();
    signal_args = gc.list(g.s_TYPE_ERROR, gc.list(g.s_OR, g.s_FIXNUM, g.s_FLOAT), Value(aux1), Value(aux2));
    GC_UNGUARD();
    TAILCALL(shared_do_raise_signal);
}

TAILCALLABLE(add1_sub1_type_error)
{
    GC_GUARD();
    signal_args = gc.list(g.s_TYPE_ERROR, gc.list(g.s_OR, g.s_FIXNUM, g.s_FLOAT), vm.param_top());
    GC_UNGUARD();
    TAILCALL(shared_do_raise_signal);
}

TAILCALLABLE(add_with_floats)
{
    // aux3 is already setup
    Float f;
    if (aux3 == 6)
    {
        f = Value(aux2).as_fixnum();
        f += Value(aux1).as_object()->to_float();
    }
    else if (aux3 == 9)
    {
        f = Value(aux1).as_fixnum();
        f += Value(aux2).as_object()->to_float();
    }
    else //if (aux3 == 12)
    {
        f = Value(aux1).as_object()->to_float();
        f += Value(aux2).as_object()->to_float();
    }
    vm.push_param(gc.alloc_object<Float>(f));
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
    ip += 1;
    aux1 = vm.pop_param();
    aux2 = vm.pop_param();

    aux3 = 0;
    aux3 += Value(aux1).is_fixnum();
    aux3 += Value(aux2).is_fixnum() * 2;
    if (aux3 == 3)
    {
        vm.push_param(Value(aux1) + Value(aux2));
        DISPATCH_NEXT;
    }

    aux3 += Value(aux1).is_type(Object_Type::Float) * 4;
    aux3 += Value(aux2).is_type(Object_Type::Float) * 8;

    if (aux3 == 6)
    {
        TAILCALL(add_with_floats);
    }
    if (aux3 == 9)
    {
        TAILCALL(add_with_floats);
    }
    if (aux3 == 12)
    {
        TAILCALL(add_with_floats);
    }
    ip -= 1; // Need to backup for the error reporting.
    TAILCALL(add_sub_type_error);
}

TAILCALLABLE(add_1_float)
{
    Float f = vm.pop_param().as_object()->to_float();
    vm.push_param(gc.alloc_object<Float>(f + 1.0));
    ip += 1;
    DISPATCH_NEXT;
    PREDICT(set_local);
}

OPCODE(add_1)
{
    if (vm.param_top().is_fixnum())
    {
        vm.param_top() += Value::wrap_fixnum(1);
        ip += 1;
        PREDICT(set_local);
        DISPATCH_NEXT;
    }
    else if (vm.param_top().is_type(Object_Type::Float))
    {
        TAILCALL(add_1_float);
    }
    else
    {
        TAILCALL(add1_sub1_type_error);
    }
}

TAILCALLABLE(sub_with_floats)
{
    // aux3 is already setup
    Float f;
    if (aux3 == 6)
    {
        f = Value(aux1).as_object()->to_float();
        f -= Value(aux2).as_fixnum();
    }
    else if (aux3 == 9)
    {
        f = Value(aux1).as_fixnum();
        f -= Value(aux2).as_object()->to_float();
    }
    else //if (aux3 == 12)
    {
        f = Value(aux1).as_object()->to_float();
        f -= Value(aux2).as_object()->to_float();
    }
    vm.push_param(gc.alloc_object<Float>(f));
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
    ip += 1;
    aux2 = vm.pop_param();
    aux1 = vm.pop_param();

    aux3 = 0;
    aux3 += Value(aux1).is_fixnum();
    aux3 += Value(aux2).is_fixnum() * 2;
    // int, int
    if (aux3 == 3)
    {
        vm.push_param(Value(aux1) - Value(aux2));
        DISPATCH_NEXT;
    }

    aux3 += Value(aux1).is_type(Object_Type::Float) * 4;
    aux3 += Value(aux2).is_type(Object_Type::Float) * 8;

    // float, int
    if (aux3 == 6)
    {
        TAILCALL(sub_with_floats);
    }
    // int, float
    if (aux3 == 9)
    {
        TAILCALL(sub_with_floats);
    }
    // float, float
    if (aux3 == 12)
    {
        TAILCALL(sub_with_floats);
    }
    ip -= 1; // Need to backup for the error reporting.
    TAILCALL(add_sub_type_error);
}

TAILCALLABLE(sub1_float)
{
    Float f = vm.pop_param().as_object()->to_float();
    vm.push_param(gc.alloc_object<Float>(f - 1.0));
    ip += 1;
    DISPATCH_NEXT;
}

OPCODE(sub_1)
{
    if (vm.param_top().is_fixnum())
    {
        vm.param_top() -= Value::wrap_fixnum(1);
        ip += 1;
        DISPATCH_NEXT;
    }
    else if (vm.param_top().is_type(Object_Type::Float))
    {
        TAILCALL(sub1_float);
    }
    else
    {
        TAILCALL(add1_sub1_type_error);
    }
}

OPCODE(dup)
{
    vm.push_param(vm.param_top());
    ip += 1;
    DISPATCH_NEXT;
}

OPCODE(raise_signal)
{
    // @Design, should we move the tag to be the first thing pushed since this just gets
    // turned into a FUNCALL? The only reason to have the tag here is for easy access.
    GC_GUARD();
    auto tag_ = vm.pop_param();
    auto nargs_ = *reinterpret_cast<const uint32_t*>(ip+1);
    signal_args = to_list(vm.m_stack_top - nargs_, nargs_);
    signal_args = gc.cons(tag_, Value(signal_args));
    GC_UNGUARD();

    TAILCALL(shared_do_raise_signal);
}

TAILCALLABLE(shared_do_raise_signal)
{
    {
        VM_State::Handler_Case restore;
        VM_State::Signal_Handler handler;
        if (vm.find_handler(first(Value(signal_args)), true, restore, handler))
        {
            vm.m_stack_top = restore.stack;
            vm.m_call_frame_top = restore.frame;
            // By default signal_args includes the handler tag and a specific handler knows its
            // own tag because it is labeled as such. In the case of a handler with the T tag it
            // is unknown so we leave it, otherwise it is removed.
            auto ctx = vm.alloc_signal_context(Value(signal_args), ip);
            if (handler.tag != g.s_T)
            {
                signal_args = cdr(Value(signal_args));
            }
            func = handler.handler;
            nargs = 1;
            vm.push_param(ctx);
            while (!Value(signal_args).is_nil())
            {
                ++nargs;
                vm.push_param(car(Value(signal_args)));
                signal_args = cdr(Value(signal_args));
            }
        }
        else
        {
            Signal_Context *ctx = nullptr;
            if (first(Value(signal_args)).is_type(Object_Type::Signal_Context))
            {
                ctx = first(Value(signal_args)).as_object()->signal_context();
            }
            auto top = vm.m_call_frame_top;
            auto bottom = vm.m_call_frame_bottom;
            vm.m_call_frame_top = vm.m_call_frame_bottom;
            vm.m_stack_top = vm.m_stack_bottom;
            throw VM_State::Signal_Exception(Value(signal_args), ip, top, bottom, ctx);
        }
    }
    [[clang::musttail]]return execute_shared_do_funcall<false>(execute_table, ip, vm, aux1, aux2, aux3);
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

const uint8_t *VM_State::execute_impl_tailcalls(const uint8_t *ip)
{
    return execute_table[*ip]((void*)execute_table.data(), ip, *this, 0, 0, 0);
}
