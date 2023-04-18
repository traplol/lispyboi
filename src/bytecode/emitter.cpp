#include "../defines.hpp"
#include "../value.hpp"
#include "../runtime_globals.hpp"
#include "../gc.hpp"
#include "../vm_state.hpp"
#include "../util.hpp"
#include "opcode.hpp"
#include "compiler.hpp"
#include "emitter.hpp"

using namespace lisp;
using namespace lisp::bytecode;

void Emitter::emit_push_literal(Value value)
{
    if (value.is_garbage_collected())
    {
        g.literal_object_slots.push_back(value);
    }

    if (value.is_nil())
    {
        emit_push_nil();
    }
    else if (value == Value::wrap_fixnum(0))
    {
        emit_push_fixnum_0();
    }
    else if (value == Value::wrap_fixnum(1))
    {
        emit_push_fixnum_1();
    }
    else
    {
        append(Opcode::op_push_value);
        append(value);
    }
}

void Emitter::emit_push_nil()
{
    append(Opcode::op_push_nil);
}

void Emitter::emit_push_fixnum_0()
{
    append(Opcode::op_push_fixnum_0);
}

void Emitter::emit_push_fixnum_1()
{
    append(Opcode::op_push_fixnum_1);
}

void Emitter::emit_funcall(uint32_t argc)
{
    append(Opcode::op_funcall);
    append(argc);
}

void Emitter::emit_gotocall(uint32_t argc)
{
    append(Opcode::op_gotocall);
    append(argc);
}

void Emitter::emit_apply(uint32_t argc)
{
    append(Opcode::op_apply);
    append(argc);
}

void Emitter::emit_return()
{
    append(Opcode::op_return);
}

int32_t Emitter::emit_jump()
{
    append(Opcode::op_jump);
    auto offset = position();
    append<uint32_t>(0xDEADBEEF);
    return offset;
}
int32_t Emitter::emit_pop_jump_if_nil()
{
    append(Opcode::op_pop_jump_if_nil);
    auto offset = position();
    append<uint32_t>(0xDEADBEEF);
    return offset;
}

void Emitter::emit_get_value(Symbol *symbol)
{
    uint32_t idx = ~0u;
    Opcode opcode;

    if (m_scope->resolve_local(symbol, idx))
    {
        if (m_scope->is_root())
        {
            opcode = Opcode::op_get_global;
        }
        else
        {
            opcode = Opcode::op_get_local;
        }
    }
    else if (m_scope->resolve_capture(symbol, idx))
    {
        opcode = Opcode::op_get_capture;
    }
    else if (m_scope->get_root()->resolve_local(symbol, idx))
    {
        opcode = Opcode::op_get_global;
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

    assert(idx != ~0u);

    append(opcode);
    append(idx);
}

void Emitter::emit_get_value(Value value)
{
    if (!symbolp(value))
    {
        fprintf(stderr, "Cannot get_value non-symbol value: %s\n", repr(value).c_str());
        abort();
    }
    else if (value == g.s_T || value.as_object()->symbol()->is_keyword())
    {
        emit_push_literal(value);
    }
    else
    {
        emit_get_value(value.as_object()->symbol());
    }
}

void Emitter::emit_set_value(Symbol *symbol)
{
    uint32_t idx = ~0u;
    Opcode opcode;

    if (m_scope->resolve_local(symbol, idx))
    {
        if (m_scope->is_root())
        {
            opcode = Opcode::op_set_global;
        }
        else
        {
            opcode = Opcode::op_set_local;
        }
    }
    else if (m_scope->resolve_capture(symbol, idx))
    {
        opcode = Opcode::op_set_capture;
    }
    else
    {
        opcode = Opcode::op_set_global;
        auto root = m_scope->get_root();
        if (!root->resolve_local(symbol, idx))
        {
            root->create_variable(symbol, &idx);
        }
    }

    assert(idx != ~0u);

    append(opcode);
    append(idx);
}

void Emitter::emit_set_value(Value value)
{
    if (!symbolp(value))
    {
        fprintf(stderr, "Cannot set_value non-symbol value: %s\n", repr(value).c_str());
        abort();
    }
    else
    {
        emit_set_value(value.as_object()->symbol());
    }
}

void Emitter::emit_function_value(Value func_val)
{
    append(Opcode::op_function_value);
    append(func_val);
}

void Emitter::emit_instantiate_closure(const Function *function)
{
    append(Opcode::op_instantiate_closure);
    append(function);
}

void Emitter::emit_close_values(uint32_t num_values)
{
    append(Opcode::op_close_values);
    append(num_values);
}

void Emitter::emit_pop()
{
    append(Opcode::op_pop);
}

void Emitter::emit_halt()
{
    append(Opcode::op_halt);
}

int32_t Emitter::emit_push_handler_case(uint32_t num_handlers)
{
    append(Opcode::op_push_handler_case);
    append(num_handlers);
    auto offset = position();
    append<uint32_t>(0xDEADBEEF);
    return offset;
}

void Emitter::emit_pop_handler_case()
{
    append(Opcode::op_pop_handler_case);
}

void Emitter::emit_raise_signal(uint32_t argc)
{
    append(Opcode::op_raise_signal);
    append(argc);
}

void Emitter::emit_cons()
{
    append(Opcode::op_cons);
}

void Emitter::emit_car()
{
    append(Opcode::op_car);
}

void Emitter::emit_cdr()
{
    append(Opcode::op_cdr);
}

void Emitter::emit_eq()
{
    append(Opcode::op_eq);
}

void Emitter::emit_rplaca()
{
    append(Opcode::op_rplaca);
}

void Emitter::emit_rplacd()
{
    append(Opcode::op_rplacd);
}

void Emitter::emit_aref()
{
    append(Opcode::op_aref);
}

void Emitter::emit_aset()
{
    append(Opcode::op_aset);
}

void Emitter::emit_add()
{
    append(Opcode::op_add);
}

void Emitter::emit_add_1()
{
    append(Opcode::op_add_1);
}

void Emitter::emit_sub()
{
    append(Opcode::op_sub);
}

void Emitter::emit_sub_1()
{
    append(Opcode::op_sub_1);
}

void Emitter::emit_debug_trap()
{
    append(Opcode::op_debug_trap);
}

int32_t Emitter::position() const
{
    return m_bytecode.size();
}

void Emitter::lock()
{
    if (m_locked)
    {
        return;
    }

    for (const auto &it : m_debug_map)
    {
        auto size = it.end - it.begin;
        auto addr = m_bytecode.data() + it.begin;
        Debug_Info::push(addr, size, it.value);
    }

    m_locked = true;
}

void Emitter::map_range_to(size_t begin, size_t end, Value expr)
{
    m_debug_map.push_back({begin, end, expr});
}

const std::vector<uint8_t> &Emitter::bytecode() const
{
    return m_bytecode;
}

std::vector<uint8_t> &&Emitter::move_bytecode()
{
    return std::move(m_bytecode);
}

void Emitter::push_labels()
{
    m_label_stack.push_back(Label_Map());
}

void Emitter::pop_labels()
{
    assert(m_label_stack.size() != 0);

    {
        auto it = m_backfills.begin();
        while (it != m_backfills.end())
        {
            int32_t label_offs;
            if (get_label(it->tag, label_offs))
            {
                auto set_offs = it->offset;
                set_raw<uint32_t>(set_offs, label_offs - (set_offs - 1));
                it = m_backfills.erase(it);
            }
            else
            {
                it++;
            }
        }
    }

    m_label_stack.pop_back();
    if (m_label_stack.size() == 0 && !m_backfills.empty())
    {
        std::vector<Value> vals;
        for (auto const &it : m_backfills)
        {
            vals.push_back(it.tag);
        }
        GC_GUARD();
        auto signal_args = gc.list(g.s_SIMPLE_ERROR,
                                   gc.alloc_string("No label tag found"),
                                   to_list(vals));
        GC_UNGUARD();
        throw VM_State::Signal_Exception(signal_args);
    }
}

bool Emitter::get_label(Value tag, int32_t &out_offset)
{
    for (auto it = m_label_stack.rbegin(); it != m_label_stack.rend(); ++it)
    {
        auto found = it->find(tag);
        if (found != it->end())
        {
            out_offset = found->second;
            return true;
        }
    }
    return false;
}

int32_t Emitter::make_label(Value tag)
{
    assert(m_label_stack.size() != 0);
    if (m_label_stack.back().find(tag) != m_label_stack.back().end())
    {
        GC_GUARD();
        auto signal_args = gc.list(g.s_SIMPLE_ERROR,
                                   gc.alloc_string("Label with tag already exists"),
                                   tag);
        GC_UNGUARD();
        throw VM_State::Signal_Exception(signal_args);
    }
    auto pos = position();
    m_label_stack.back()[tag] = pos;
    return pos;
}

void Emitter::backfill_label(int32_t offset, Value tag)
{
    assert(m_label_stack.size() != 0);
    m_backfills.push_back({tag, offset});
}

compiler::Scope *Emitter::scope() const
{
    return m_scope;
}
