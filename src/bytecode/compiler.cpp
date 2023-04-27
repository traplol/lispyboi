#include <vector>
#include <unordered_map>

#include "../defines.hpp"
#include "../value.hpp"
#include "../lisp_reference_types.hpp"
#include "../runtime_globals.hpp"
#include "../util.hpp"
#include "../gc.hpp"
#include "../vm_state.hpp"
#include "compiler.hpp"
#include "emitter.hpp"
#include "bc_emitter.hpp"
#include "list_emitter.hpp"

namespace lisp
{
namespace bytecode
{

std::vector<std::pair<Debug_Info::Region, Value>> Debug_Info::m_bytecode_address_expr_map;
std::vector<const Function*> Debug_Info::m_functions;

}
namespace compiler
{

static
bool constantp(Value expr)
{
    if (expr.is_type(Object_Type::Symbol))
    {
        if (expr == g.s_T)
        {
            return true;
        }
        if (expr.as_object()->symbol()->is_keyword())
        {
            return true;
        }
        return false;
    }
    if (expr.is_cons())
    {
        auto f = car(expr);
        if (f == g.s_QUOTE ||
            f == g.s_FUNCTION ||
            f == g.s_pLAMBDA)
        {
            return true;
        }
        return false;
    }
    return true;
}

static
bool effect_free(Value expr)
{
    return expr.is_type(Object_Type::Symbol)
        || constantp(expr);
}

static
void compile_get_value(bytecode::Emitter &e, Scope *scope, Symbol *symbol)
{
    uint32_t idx = ~0u;

    assert(symbol);

    if (scope->resolve_local(symbol, idx))
    {
        if (scope->is_root())
        {
            assert(idx != ~0u);
            e.emit_get_global(idx);
        }
        else
        {
            assert(idx != ~0u);
            e.emit_get_local(idx);
        }
    }
    else if (scope->resolve_capture(symbol, idx))
    {
        assert(idx != ~0u);
        e.emit_get_capture(idx);
    }
    else if (scope->get_root()->resolve_local(symbol, idx))
    {
        assert(idx != ~0u);
        e.emit_get_global(idx);
    }
    else
    {
        GC_GUARD();
        auto signal_args = gc.list(g.s_SIMPLE_ERROR,
                                   gc.alloc_string("Undefined symbol"),
                                   gc.alloc_string(symbol->qualified_name()));
        GC_UNGUARD();
        throw VM_State::Signal_Exception(signal_args);
    }
}

static
void compile_get_value(bytecode::Emitter &e, Scope *scope, Value value)
{
    if (!symbolp(value))
    {
        fprintf(stderr, "Cannot get_value non-symbol value: %s\n", repr(value).c_str());
        abort();
    }
    else if (value == g.s_T || value.as_object()->symbol()->is_keyword())
    {
        e.emit_push_value(value);
    }
    else
    {
        compile_get_value(e, scope, value.as_object()->symbol());
    }
}

static
void compile_set_value(bytecode::Emitter &e, Scope *scope, Symbol *symbol)
{
    uint32_t idx = ~0u;

    if (scope->resolve_local(symbol, idx))
    {
        if (scope->is_root())
        {
            assert(idx != ~0u);
            e.emit_set_global(idx);
        }
        else
        {
            assert(idx != ~0u);
            e.emit_set_local(idx);
        }
    }
    else if (scope->resolve_capture(symbol, idx))
    {
        assert(idx != ~0u);
        e.emit_set_capture(idx);
    }
    else
    {
        auto root = scope->get_root();
        if (!root->resolve_local(symbol, idx))
        {
            root->create_variable(symbol, &idx);
        }
        assert(idx != ~0u);
        e.emit_set_global(idx);
    }
}

static
void compile_set_value(bytecode::Emitter &e, Scope *scope, Value value)
{
    if (!symbolp(value))
    {
        fprintf(stderr, "Cannot set_value non-symbol value: %s\n", repr(value).c_str());
        abort();
    }
    else
    {
        compile_set_value(e, scope, value.as_object()->symbol());
    }
}

static
void compile_body(bytecode::Emitter &e, Scope *scope, Value body, bool tail_position)
{
    while (!cdr(body).is_nil())
    {
        if (!effect_free(car(body)))
        {
            compile(e, scope, car(body), false, false);
            e.emit_pop();
        }
        body = cdr(body);
    }
    compile(e, scope, car(body), false, tail_position);
}

static
void compile_function_le(bytecode::Emitter &e, Scope *scope, Value expr, bool macro, bool toplevel)
{
    auto name = second(expr);
    auto lambda_list = macro ? third(expr) : second(expr);
    auto body = macro ? cdddr(expr) : cddr(expr);

    auto func_scope = scope->push_scope();
    bytecode::List_Emitter function_emitter;
    // Optionals are a little tricky because we allow for any expression to be the default value
    // to an optional, this even means that a default value may refer to an earlier parameter eg:
    //     (defun substring (string start &optional (end (length string))) ...)
    // In this example, end has not only a default value but it's a call to a function using a
    // local variable.
    //
    // We'll solve this by generating the equivalent to a bunch of SETQs for the defaults and
    // storing the address of each one, then at runtime we'll figure out which one of these
    // to jump to.
    auto cur = lambda_list;
    std::vector<const Symbol*> params;
    bool has_optionals = false;
    bool has_rest = false;
    size_t optionals_start_at = 0;
    while (!cur.is_nil())
    {
        auto sym = car(cur);
        if (sym == g.s_aOPTIONAL)
        {
            cur = cdr(cur);
            has_optionals = true;
            break;
        }
        if (sym == g.s_aREST || sym == g.s_aBODY)
        {
            has_rest = true;
            break;
        }
        optionals_start_at++;
        if (!symbolp(sym))
        {
            GC_GUARD();
            auto signal_args = gc.list(g.s_SIMPLE_ERROR,
                                       gc.alloc_string("Non-symbol parameter in lambda list"),
                                       sym);
            GC_UNGUARD();
            throw VM_State::Signal_Exception(signal_args);
        }
        params.push_back(sym.as_object()->symbol());
        cur = cdr(cur);
    }

    for (auto const symbol : params)
    {
        func_scope->create_variable(symbol);
    }

    std::vector<void*> optional_offsets;
    for (size_t i = 0; i < optionals_start_at; ++i)
    {
        optional_offsets.push_back(0);
    }

    if (has_optionals || has_rest)
    {
        // at this point cur is now pointing to optionals
        while (!cur.is_nil())
        {
            auto param = first(cur);
            if (param == g.s_aREST || param == g.s_aBODY)
            {
                param = second(cur);
                cur = Value::nil();
                has_rest = true;
            }

            optional_offsets.push_back(function_emitter.internal_label("optionals"));

            if (param.is_cons())
            {
                auto symbol = first(param).as_object()->symbol();
                func_scope->create_variable(symbol);
                params.push_back(symbol);

                compile(function_emitter, func_scope, second(param), false);
                compile_set_value(function_emitter, func_scope, first(param));
                function_emitter.emit_pop();
            }
            else
            {
                auto symbol = param.as_object()->symbol();
                func_scope->create_variable(symbol);
                params.push_back(symbol);

                function_emitter.emit_push_nil();
                compile_set_value(function_emitter, func_scope, param);
                function_emitter.emit_pop();
            }
            cur = cdr(cur);
        }
    }
    else
    {
        assert(optionals_start_at == params.size());
    }

    auto main_entry_offset = function_emitter.internal_label("entrypoint");
    compile_body(function_emitter, func_scope, body, true);
    function_emitter.emit_return();
    //function_emitter.lock(); // the function MUST be locked before we can resolve captures or move bytecode.
    std::vector<Function::Capture_Offset> capture_offsets;
    for (auto const &cap : func_scope->capture_info())
    {
        capture_offsets.push_back({
                cap.index,
                cap.is_local,
                cap.symbol->qualified_name()
            });
    }

    //auto const *function = new Function(std::move(function_emitter.move_bytecode()),
    //                                    std::move(optional_offsets),
    //                                    std::move(params),
    //                                    std::move(capture_offsets),
    //                                    main_entry_offset,
    //                                    optionals_start_at,
    //                                    // using the max number of locals instead of the function's arity
    //                                    // because we may inline immediate lambda calls which expand the
    //                                    // number of local variables but not change the arity of the
    //                                    // outer function.
    //                                    func_scope->stack_space_needed(),
    //                                    has_rest,
    //                                    has_optionals);

    if (macro)
    {
        //auto obj = gc.alloc_object_unmanaged<Closure>(function);
        //g.macros[name.as_object()->symbol()] = obj;
    }
    else
    {
        //e.emit_instantiate_closure(function);
    }
    //bytecode::Debug_Info::track_function(function);
    // We created it in this function, so needs to be deleted here.
    std::cout << "List Emitter for: " << repr(expr) << "\n";
    function_emitter.pp();
    delete func_scope;
}

static
void compile_function(bytecode::Emitter &e, Scope *scope, Value expr, bool macro, bool toplevel)
{
    {
        compile_function_le(e, scope, expr, macro, toplevel);
    }
    auto name = second(expr);
    auto lambda_list = macro ? third(expr) : second(expr);
    auto body = macro ? cdddr(expr) : cddr(expr);

    auto func_scope = scope->push_scope();
    bytecode::BC_Emitter function_emitter;
    // Optionals are a little tricky because we allow for any expression to be the default value
    // to an optional, this even means that a default value may refer to an earlier parameter eg:
    //     (defun substring (string start &optional (end (length string))) ...)
    // In this example, end has not only a default value but it's a call to a function using a
    // local variable.
    //
    // We'll solve this by generating the equivalent to a bunch of SETQs for the defaults and
    // storing the address of each one, then at runtime we'll figure out which one of these
    // to jump to.
    auto cur = lambda_list;
    std::vector<const Symbol*> params;
    bool has_optionals = false;
    bool has_rest = false;
    size_t optionals_start_at = 0;
    while (!cur.is_nil())
    {
        auto sym = car(cur);
        if (sym == g.s_aOPTIONAL)
        {
            cur = cdr(cur);
            has_optionals = true;
            break;
        }
        if (sym == g.s_aREST || sym == g.s_aBODY)
        {
            has_rest = true;
            break;
        }
        optionals_start_at++;
        if (!symbolp(sym))
        {
            GC_GUARD();
            auto signal_args = gc.list(g.s_SIMPLE_ERROR,
                                       gc.alloc_string("Non-symbol parameter in lambda list"),
                                       sym);
            GC_UNGUARD();
            throw VM_State::Signal_Exception(signal_args);
        }
        params.push_back(sym.as_object()->symbol());
        cur = cdr(cur);
    }

    for (auto const symbol : params)
    {
        func_scope->create_variable(symbol);
    }

    std::vector<uint32_t> optional_offsets;
    for (size_t i = 0; i < optionals_start_at; ++i)
    {
        optional_offsets.push_back(0);
    }

    if (has_optionals || has_rest)
    {
        // at this point cur is now pointing to optionals
        while (!cur.is_nil())
        {
            auto param = first(cur);
            if (param == g.s_aREST || param == g.s_aBODY)
            {
                param = second(cur);
                cur = Value::nil();
                has_rest = true;
            }

            optional_offsets.push_back(function_emitter.position());

            if (param.is_cons())
            {
                auto symbol = first(param).as_object()->symbol();
                func_scope->create_variable(symbol);
                params.push_back(symbol);

                compile(function_emitter, func_scope, second(param), false);
                compile_set_value(function_emitter, func_scope, first(param));
                function_emitter.emit_pop();
            }
            else
            {
                auto symbol = param.as_object()->symbol();
                func_scope->create_variable(symbol);
                params.push_back(symbol);

                function_emitter.emit_push_nil();
                compile_set_value(function_emitter, func_scope, param);
                function_emitter.emit_pop();
            }
            cur = cdr(cur);
        }
    }
    else
    {
        assert(optionals_start_at == params.size());
    }

    auto main_entry_offset = function_emitter.position();
    compile_body(function_emitter, func_scope, body, true);
    function_emitter.emit_return();
    function_emitter.lock(); // the function MUST be locked before we can resolve captures or move bytecode.
    std::vector<Function::Capture_Offset> capture_offsets;
    for (auto const &cap : func_scope->capture_info())
    {
        capture_offsets.push_back({
                cap.index,
                cap.is_local,
                cap.symbol->qualified_name()
            });
    }

    auto const *function = new Function(std::move(function_emitter.move_bytecode()),
                                        std::move(optional_offsets),
                                        std::move(params),
                                        std::move(capture_offsets),
                                        main_entry_offset,
                                        optionals_start_at,
                                        // using the max number of locals instead of the function's arity
                                        // because we may inline immediate lambda calls which expand the
                                        // number of local variables but not change the arity of the
                                        // outer function.
                                        func_scope->stack_space_needed(),
                                        has_rest,
                                        has_optionals);

    if (macro)
    {
        auto obj = gc.alloc_object_unmanaged<Closure>(function);
        g.macros[name.as_object()->symbol()] = obj;
    }
    else
    {
        e.emit_instantiate_closure(function);
    }
    bytecode::Debug_Info::track_function(function);
    // We created it in this function, so needs to be deleted here.
    delete func_scope;
}


static
void compile_function_call(bytecode::Emitter &e, Scope *scope, Value func, Value args, bool toplevel, bool tail_position, bool funcall)
{
    if (func.is_cons() && first(func) == g.s_pLAMBDA)
    {
        if (second(func).is_nil())
        {
            // calling a lambda that takes no arguments is directly inlinable,
            // no call needed... :)
            compile_body(e, scope, cddr(func), tail_position);
            return;
        }
        else if (!toplevel)
        {
            // and in the case of a lambda instead of instantiate + funcall we just inline into the current
            // stack frame.
            // @TODO: although it's unlikely to have &optional and &rest here, we should still support it
            // but for now we'll just not do the optimization in that case.
            bool do_opt = true;
            auto params = second(func);
            std::vector<Value> symbols;
            while (!params.is_nil())
            {
                if (symbolp(car(params)))
                {
                    symbols.push_back(car(params));
                }
                else
                {
                    do_opt = false;
                    break;
                }
                params = cdr(params);
            }

            if (do_opt)
            {
                size_t i = 0;
                while (!args.is_nil())
                {
                    i++;
                    compile(e, scope, car(args), false);
                    args = cdr(args);
                }
                for (; i < symbols.size(); ++i)
                {
                    e.emit_push_nil();
                }
                std::vector<Variable*> vars;
                for (auto symbol_value : symbols)
                {
                    auto symbol = symbol_value.as_object()->symbol();
                    auto var = scope->create_variable(symbol);
                    vars.push_back(var);
                }
                for (auto it = symbols.rbegin(); it != symbols.rend(); ++it)
                {
                    compile_set_value(e, scope, *it);
                    e.emit_pop();
                }
                compile_body(e, scope, cddr(func), tail_position);
                // If we're in the tail position, then the gotocall will handle cleaning up the stack
                // so there's no need to emit these instructions.
                if (!tail_position)
                {
                    if (scope->capture_info().size() != 0)
                    {
                        e.emit_close_values(symbols.size());
                    }
                }
                scope->pop_variables(symbols.size());
                return;
            }
        }
    }

    uint32_t nargs = 0;
    while (!args.is_nil())
    {
        compile(e, scope, car(args), toplevel);
        nargs++;
        args = cdr(args);
    }
    if (func.is_cons() && first(func) == g.s_pLAMBDA)
    {
        compile(e, scope, func, toplevel);
    }
    else if (funcall)
    {
        if (symbolp(func))
        {
            compile_get_value(e, scope, func);
        }
        else
        {
            compile(e, scope, func, toplevel);
        }
    }
    else
    {
        e.emit_push_value(func);
    }

    if (tail_position)
    {
        e.emit_gotocall(nargs);
    }
    else
    {
        e.emit_funcall(nargs);
    }
}


void compile(bytecode::Emitter &e, Scope *scope, Value expr, bool toplevel, bool tail_position)
{
    if (expr.is_cons())
    {
        const auto thing = first(expr);
        //const auto begin = e.position();
        //const auto saved_expr = expr;
        if (thing == g.s_QUOTE)
        {
            e.emit_push_value(second(expr));
        }
        else if (thing == g.s_pGO)
        {
            auto id = e.emit_jump();
            e.backfill_label(id, second(expr));
        }
        else if (thing == g.s_pTAGBODY)
        {
            e.push_labels();
            e.emit_push_nil();
            auto body = cdr(expr);
            while (!body.is_nil())
            {
                auto it = car(body);
                if (it.is_cons())
                {
                    compile(e, scope, it, toplevel, false);
                    e.emit_pop();
                }
                else
                {
                    e.user_label(it);
                }
                body = cdr(body);
            };
            e.pop_labels();
        }
        else if (thing == g.s_IF)
        {
            auto test = second(expr);
            auto consequence = third(expr);
            auto alternative = fourth(expr);
            if (constantp(test))
            {
                if (test.is_nil())
                {
                    compile(e, scope, alternative, toplevel, tail_position);
                }
                else
                {
                    compile(e, scope, consequence, toplevel, tail_position);
                }
            }
            else
            {
                compile(e, scope, test, toplevel);
                auto alt_id = e.emit_pop_jump_if_nil();
                compile(e, scope, consequence, toplevel, tail_position);
                if (tail_position)
                {
                    e.emit_return();
                    auto alt_label = e.internal_label();
                    compile(e, scope, alternative, toplevel, tail_position);

                    e.resolve_jump(alt_id, alt_label);
                }
                else
                {
                    auto out_id = e.emit_jump();
                    auto alt_label = e.internal_label();
                    compile(e, scope, alternative, toplevel, tail_position);

                    e.resolve_jump_to_current(out_id);
                    e.resolve_jump(alt_id, alt_label);
                }
            }
        }
        else if (thing == g.s_pDEFINE_MACRO)
        {
            compile_function(e, scope, expr, true, true);
        }
        else if (thing == g.s_pLAMBDA)
        {
            compile_function(e, scope, expr, false, toplevel);
        }
        else if (thing == g.s_pSETQ)
        {
            compile(e, scope, third(expr), toplevel);
            if (symbolp(second(expr)))
            {
                compile_set_value(e, scope, second(expr));
            }
            else
            {
                fprintf(stderr, "Cannot %%SETQ a non-symbol: %s\n", repr(second(expr)).c_str());
            }
        }
        else if (thing == g.s_pHANDLER_CASE)
        {
            auto form = second(expr);
            auto handlers = cddr(expr);
            uint32_t nhandlers = 0;
            while (!handlers.is_nil())
            {
                nhandlers++;
                auto handler = car(handlers);
                auto handler_tag = first(handler);
                compile_function(e, scope, handler, false, toplevel);
                e.emit_push_value(handler_tag);
                if (handler_tag == g.s_T)
                {
                    //if (!cdr(handlers).is_nil())
                    //{
                    //    fprintf(stderr, "WARNING: Unreachable code in HANDLER-CASE\n");
                    //}
                    break;
                }
                handlers = cdr(handlers);
            }
            auto id = e.emit_push_handler_case(nhandlers);
            compile(e, scope, form, toplevel);
            e.emit_pop_handler_case();
            e.close_push_handler_case(id);
        }
        else if (thing == g.s_FUNCTION)
        {
            auto thing = second(expr);
            if (thing.is_cons())
            {
                if (first(thing) == g.s_pLAMBDA)
                {
                    compile(e, scope, thing, toplevel);
                }
                else
                {
                    // ???
                }
            }
            else
            {
                e.emit_function_value(second(expr));
            }
        }
        else if (thing == g.s_pFUNCALL)
        {
            compile_function_call(e, scope, second(expr), cddr(expr), toplevel, tail_position, true);
        }
        else if (thing == g.s_pCONS)
        {
            compile(e, scope, second(expr), toplevel);
            compile(e, scope, third(expr), toplevel);
            e.emit_cons();
        }
        else if (thing == g.s_pCAR)
        {
            compile(e, scope, second(expr), toplevel);
            e.emit_car();
        }
        else if (thing == g.s_pCDR)
        {
            compile(e, scope, second(expr), toplevel);
            e.emit_cdr();
        }
        else if (thing == g.s_pSIGNAL)
        {
            uint32_t nargs = 0;
            auto tag = second(expr);
            auto args = cddr(expr);
            while (!args.is_nil())
            {
                compile(e, scope, car(args), toplevel);
                args = cdr(args);
                nargs++;
            }
            compile(e, scope, tag, toplevel);
            e.emit_raise_signal(nargs);
        }
        else if (thing == g.s_pEQ)
        {
            compile(e, scope, second(expr), toplevel);
            compile(e, scope, third(expr), toplevel);
            e.emit_eq();
        }
        else if (thing == g.s_pRPLACA)
        {
            compile(e, scope, second(expr), toplevel);
            compile(e, scope, third(expr), toplevel);
            e.emit_rplaca();
        }
        else if (thing == g.s_pRPLACD)
        {
            compile(e, scope, second(expr), toplevel);
            compile(e, scope, third(expr), toplevel);
            e.emit_rplacd();
        }
        else if (thing == g.s_pAREF)
        {
            compile(e, scope, second(expr), toplevel);
            compile(e, scope, third(expr), toplevel);
            e.emit_aref();
        }
        else if (thing == g.s_pASET)
        {
            compile(e, scope, second(expr), toplevel);
            compile(e, scope, third(expr), toplevel);
            compile(e, scope, fourth(expr), toplevel);
            e.emit_aset();
        }
        else if (thing == g.s_pDEBUGGER)
        {
            compile(e, scope, second(expr), toplevel);
            e.emit_debug_trap();
        }
        else if (thing == g.s_pAPPLY)
        {
            uint32_t nargs = 0;
            auto args = cddr(expr);
            while (!args.is_nil())
            {
                compile(e, scope, car(args), toplevel);
                nargs++;
                args = cdr(args);
            }
            compile(e, scope, second(expr), toplevel);
            e.emit_apply(nargs);
        }
        else
        {
            if (thing == g.s_pplus && length(expr) == 3) // add a, b
            {
                auto a = second(expr);
                auto b = third(expr);
                if (a.is_fixnum() && b.is_fixnum())
                {
                    e.emit_push_value(a + b);
                }
                else if (a == Value::wrap_fixnum(1))
                {
                    compile(e, scope, b, toplevel);
                    e.emit_add_1();
                }
                else if (b == Value::wrap_fixnum(1))
                {
                    compile(e, scope, a, toplevel);
                    e.emit_add_1();
                }
                else
                {
                    compile(e, scope, a, toplevel);
                    compile(e, scope, b, toplevel);
                    e.emit_add();
                }
            }
            else if (thing == g.s_pminus && length(expr) == 3) // sub a, b
            {
                auto a = second(expr);
                auto b = third(expr);
                if (a.is_fixnum() && b.is_fixnum())
                {
                    e.emit_push_value(a - b);
                }
                else if (b == Value::wrap_fixnum(1))
                {
                    compile(e, scope, a, toplevel);
                    e.emit_sub_1();
                }
                else
                {
                    compile(e, scope, a, toplevel);
                    compile(e, scope, b, toplevel);
                    e.emit_sub();
                }
            }
            else
            {
                compile_function_call(e, scope, first(expr), rest(expr), toplevel, tail_position, false);
            }
        }
        // FIXME: I think this was supposed to be a way to map a bytecode address to an expressed...
        //if (thing != g.s_QUOTE && thing != g.s_FUNCTION)
        //{
        //    auto end = e.position();
        //    e.map_range_to(begin, end, saved_expr);
        //}
    }
    else if (symbolp(expr))
    {
        compile_get_value(e, scope, expr);
    }
    else
    {
        e.emit_push_value(expr);
    }
}

bool Variable::is_global() const
{
    return m_scope->is_root();
}

Scope *THE_ROOT_SCOPE;

}

bool bytecode::Debug_Info::find(const void *address, Value &out_expr)
{
    size_t size = ~0ull;
    bool found = false;
    for (auto it = m_bytecode_address_expr_map.rbegin();
            it != m_bytecode_address_expr_map.rend();
            ++it)
    {
        if (it->first.contains(address))
        {
            found = true;
            if (it->first.size() < size)
            {
                size = it->first.size();
                out_expr = it->second;
            }
        }
    }
    return found;
}

bool bytecode::Debug_Info::find_function(const void *address, const Function **out_function)
{
    for (auto it : m_functions)
    {
        Region r(it->begin(), it->size());
        if (r.contains(address))
        {
            *out_function = it;
            return true;
        }
    }
    return false;
}

void bytecode::Debug_Info::push(const void *start, size_t size, Value expr)
{
    Region r(start, size);
    m_bytecode_address_expr_map.push_back({r, expr});
}

void bytecode::Debug_Info::track_function(const Function *function)
{
    if (function)
    {
        m_functions.push_back(function);
    }
}
}
