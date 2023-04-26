#include "../defines.hpp"
#include "../value.hpp"
#include "../runtime_globals.hpp"
#include "../gc.hpp"
#include "../vm_state.hpp"
#include "../util.hpp"
#include "opcode.hpp"
#include "compiler.hpp"
#include "bc_emitter.hpp"

using namespace lisp;
using namespace lisp::bytecode;

void BC_Emitter::emit_push_value(Value value)
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

void BC_Emitter::emit_push_nil()
{
    append(Opcode::op_push_nil);
}

void BC_Emitter::emit_push_fixnum_0()
{
    append(Opcode::op_push_fixnum_0);
}

void BC_Emitter::emit_push_fixnum_1()
{
    append(Opcode::op_push_fixnum_1);
}

void BC_Emitter::emit_funcall(uint32_t argc)
{
    append(Opcode::op_funcall);
    append(argc);
}

void BC_Emitter::emit_gotocall(uint32_t argc)
{
    append(Opcode::op_gotocall);
    append(argc);
}

void BC_Emitter::emit_apply(uint32_t argc)
{
    append(Opcode::op_apply);
    append(argc);
}

void BC_Emitter::emit_return()
{
    append(Opcode::op_return);
}

void *BC_Emitter::emit_jump(void *destination)
{
    append(Opcode::op_jump);
    auto offset = position();
    append<int32_t>(reinterpret_cast<intptr_t>(destination));
    return reinterpret_cast<void*>(offset);
}

void *BC_Emitter::emit_jump()
{
    return emit_jump(reinterpret_cast<void*>(0xDEADBEEF));
}

void *BC_Emitter::emit_pop_jump_if_nil(void *destination)
{
    append(Opcode::op_pop_jump_if_nil);
    auto offset = position();
    append<int32_t>(reinterpret_cast<intptr_t>(destination));
    return reinterpret_cast<void*>(offset);
}

void *BC_Emitter::emit_pop_jump_if_nil()
{
    return emit_pop_jump_if_nil(reinterpret_cast<void*>(0xDEADBEEF));
}

void BC_Emitter::resolve_jump(void *branch_id, void *destination)
{
    auto br = reinterpret_cast<intptr_t>(branch_id);
    auto dest = reinterpret_cast<intptr_t>(destination);
    set_raw<int32_t>(br, dest - (br - 1));
}

void BC_Emitter::resolve_jump_to_current(void *branch_id)
{
    resolve_jump(branch_id, reinterpret_cast<void*>(position()));
}

void BC_Emitter::emit_get_global(uint32_t idx)
{
    append(Opcode::op_get_global);
    append(idx);
}

void BC_Emitter::emit_get_local(uint32_t idx)
{
    append(Opcode::op_get_local);
    append(idx);
}

void BC_Emitter::emit_get_capture(uint32_t idx)
{
    append(Opcode::op_get_capture);
    append(idx);
}

void BC_Emitter::emit_set_global(uint32_t idx)
{
    append(Opcode::op_set_global);
    append(idx);
}

void BC_Emitter::emit_set_local(uint32_t idx)
{
    append(Opcode::op_set_local);
    append(idx);
}

void BC_Emitter::emit_set_capture(uint32_t idx)
{
    append(Opcode::op_set_capture);
    append(idx);
}

void BC_Emitter::emit_function_value(Value func_val)
{
    append(Opcode::op_function_value);
    append(func_val);
}

void BC_Emitter::emit_instantiate_closure(const Function *function)
{
    append(Opcode::op_instantiate_closure);
    append(function);
}

void BC_Emitter::emit_close_values(uint32_t num_values)
{
    append(Opcode::op_close_values);
    append(num_values);
}

void BC_Emitter::emit_pop()
{
    append(Opcode::op_pop);
}

void BC_Emitter::emit_halt()
{
    append(Opcode::op_halt);
}

void *BC_Emitter::emit_push_handler_case(uint32_t num_handlers)
{
    append(Opcode::op_push_handler_case);
    append(num_handlers);
    auto offset = position();
    append<int32_t>(0xDEADBEEF);
    return reinterpret_cast<void*>(offset);
}

void BC_Emitter::close_push_handler_case(void *handler_id)
{
    auto id = reinterpret_cast<intptr_t>(handler_id);
    auto dest = position() - id;
    dest += (sizeof(uint32_t) + 1);
    set_raw<int32_t>(id, dest);
}

void BC_Emitter::emit_pop_handler_case()
{
    append(Opcode::op_pop_handler_case);
}

void BC_Emitter::emit_raise_signal(uint32_t argc)
{
    append(Opcode::op_raise_signal);
    append(argc);
}

void BC_Emitter::emit_cons()
{
    append(Opcode::op_cons);
}

void BC_Emitter::emit_car()
{
    append(Opcode::op_car);
}

void BC_Emitter::emit_cdr()
{
    append(Opcode::op_cdr);
}

void BC_Emitter::emit_eq()
{
    append(Opcode::op_eq);
}

void BC_Emitter::emit_rplaca()
{
    append(Opcode::op_rplaca);
}

void BC_Emitter::emit_rplacd()
{
    append(Opcode::op_rplacd);
}

void BC_Emitter::emit_aref()
{
    append(Opcode::op_aref);
}

void BC_Emitter::emit_aset()
{
    append(Opcode::op_aset);
}

void BC_Emitter::emit_add()
{
    append(Opcode::op_add);
}

void BC_Emitter::emit_add_1()
{
    append(Opcode::op_add_1);
}

void BC_Emitter::emit_sub()
{
    append(Opcode::op_sub);
}

void BC_Emitter::emit_sub_1()
{
    append(Opcode::op_sub_1);
}

void BC_Emitter::emit_debug_trap()
{
    append(Opcode::op_debug_trap);
}

int32_t BC_Emitter::position() const
{
    return m_bytecode.size();
}

void BC_Emitter::lock()
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

void BC_Emitter::map_range_to(size_t begin, size_t end, Value expr)
{
    m_debug_map.push_back({begin, end, expr});
}

const std::vector<uint8_t> &BC_Emitter::bytecode() const
{
    return m_bytecode;
}

std::vector<uint8_t> &&BC_Emitter::move_bytecode()
{
    return std::move(m_bytecode);
}

void BC_Emitter::push_labels()
{
    m_label_stack.push_back(Label_Map());
}

void BC_Emitter::pop_labels()
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

bool BC_Emitter::get_label(Value tag, int32_t &out_offset)
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

void *BC_Emitter::user_label(Value tag)
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
    return reinterpret_cast<void*>(pos);
}

void BC_Emitter::backfill_label(void *branch_id, Value tag)
{
    auto offset = reinterpret_cast<intptr_t>(branch_id);
    assert(m_label_stack.size() != 0);
    m_backfills.push_back({tag, static_cast<int32_t>(offset)});
}

void *BC_Emitter::internal_label()
{
    return reinterpret_cast<void*>(position());
}
