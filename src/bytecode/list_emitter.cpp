#include <iostream>
#include <iomanip>
#include <map>
#include "../defines.hpp"
#include "../vm_state.hpp"
#include "list_emitter.hpp"
#include "bc_emitter.hpp"

#define OPT_FOLD_CONSTANT_ADDS 1
#define OPT_FOLD_CONSTANT_SUBS 1
#define OPT_REMOVE_NOOP_PUSH_POPS 1
#define OPT_CONVERT_GET_SEQUENCES_TO_DUPS 1

using namespace lisp;
using namespace lisp::bytecode;

void List_Emitter::append(Bytecode_List *node)
{
    if (m_head == nullptr)
    {
        m_head = node;
        m_tail = m_head;
    }
    else if (node != nullptr)
    {
        m_tail->next = node;
        node->prev = m_tail;
        m_tail = node;
    }
}

void List_Emitter::emit_push_value(Value value)
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_push_value;
    node->operands[0].value = value;
    append(node);
}

void List_Emitter::emit_push_nil()
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_push_nil;
    append(node);
}

void List_Emitter::emit_push_fixnum_0()
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_push_fixnum_0;
    append(node);
}

void List_Emitter::emit_push_fixnum_1()
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_push_fixnum_1;
    append(node);
}

void List_Emitter::emit_funcall(uint32_t argc)
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_funcall;
    node->operands[0].num = argc;
    append(node);
}
void List_Emitter::emit_gotocall(uint32_t argc)
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_gotocall;
    node->operands[0].num = argc;
    append(node);
}
void List_Emitter::emit_apply(uint32_t argc)
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_apply;
    node->operands[0].num = argc;
    append(node);
}
void List_Emitter::emit_return()
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_return;
    append(node);
}

void List_Emitter::emit_get_global(uint32_t idx)
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_get_global;
    node->operands[0].num = idx;
    append(node);
}

void List_Emitter::emit_get_local(uint32_t idx)
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_get_local;
    node->operands[0].num = idx;
    append(node);
}

void List_Emitter::emit_get_capture(uint32_t idx)
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_get_capture;
    node->operands[0].num = idx;
    append(node);
}

void List_Emitter::emit_set_global(uint32_t idx)
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_set_global;
    node->operands[0].num = idx;
    append(node);
}

void List_Emitter::emit_set_local(uint32_t idx)
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_set_local;
    node->operands[0].num = idx;
    append(node);
}

void List_Emitter::emit_set_capture(uint32_t idx)
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_set_capture;
    node->operands[0].num = idx;
    append(node);
}
void List_Emitter::emit_function_value(Value func_val)
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_function_value;
    node->operands[0].value = func_val;
    append(node);
}
void List_Emitter::emit_instantiate_closure(const Function *function)
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_instantiate_closure;
    node->operands[0].function = function;
    append(node);
}
void List_Emitter::emit_close_values(uint32_t num_values)
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_close_values;
    append(node);
}
void List_Emitter::emit_pop()
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_pop;
    append(node);
}
void List_Emitter::emit_halt()
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_halt;
    append(node);
}
void List_Emitter::emit_pop_handler_case()
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_pop_handler_case;
    append(node);
}
void List_Emitter::emit_raise_signal(uint32_t argc)
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_raise_signal;
    node->operands[0].num = argc;
    append(node);
}
void List_Emitter::emit_cons()
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_cons;
    append(node);
}
void List_Emitter::emit_car()
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_car;
    append(node);
}
void List_Emitter::emit_cdr()
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_cdr;
    append(node);
}
void List_Emitter::emit_eq()
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_eq;
    append(node);
}
void List_Emitter::emit_rplaca()
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_rplaca;
    append(node);
}
void List_Emitter::emit_rplacd()
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_rplacd;
    append(node);
}
void List_Emitter::emit_aref()
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_aref;
    append(node);
}
void List_Emitter::emit_aset()
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_aset;
    append(node);
}
void List_Emitter::emit_add()
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_add;
    append(node);
}
void List_Emitter::emit_add_1()
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_add_1;
    append(node);
}
void List_Emitter::emit_sub()
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_sub;
    append(node);
}
void List_Emitter::emit_sub_1()
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_sub_1;
    append(node);
}
void List_Emitter::emit_dup()
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_dup;
    append(node);
}
void List_Emitter::emit_debug_trap()
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_debug_trap;
    append(node);
}

void *List_Emitter::emit_jump(void *destination)
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_jump;
    node->operands[0].destination = reinterpret_cast<Bytecode_List*>(destination);
    append(node);
    return node;
}

void *List_Emitter::emit_jump()
{
    return emit_jump(nullptr);
}

void *List_Emitter::emit_pop_jump_if_nil(void *destination)
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_pop_jump_if_nil;
    node->operands[0].destination = reinterpret_cast<Bytecode_List*>(destination);
    append(node);
    return node;
}

void *List_Emitter::emit_pop_jump_if_nil()
{
    return emit_pop_jump_if_nil(nullptr);
}

void List_Emitter::push_labels()
{
    m_label_stack.push_back(Label_Map());
}

void List_Emitter::pop_labels()
{
    assert(m_label_stack.size() != 0);

    {
        auto it = m_backfills.begin();
        while (it != m_backfills.end())
        {
            if (auto label = get_label(it->tag))
            {
                it->branch->operands[0].destination = label;
                it = m_backfills.erase(it);
            }
            else
            {
                it++;
            }
        }
    }

    m_label_stack.pop_back();
    //if (m_label_stack.size() == 0 && !m_backfills.empty())
    //{
    //    std::vector<Value> vals;
    //    for (auto const &it : m_backfills)
    //    {
    //        vals.push_back(it.tag);
    //    }
    //    GC_GUARD();
    //    auto signal_args = gc.list(g.s_SIMPLE_ERROR,
    //                               gc.alloc_string("No label tag found"),
    //                               to_list(vals));
    //    GC_UNGUARD();
    //    throw VM_State::Signal_Exception(signal_args);
    //}
}

Bytecode_List *List_Emitter::get_label(Value tag)
{
    for (auto it = m_label_stack.rbegin(); it != m_label_stack.rend(); ++it)
    {
        auto found = it->find(tag);
        if (found != it->end())
        {
            return found->second;
        }
    }
    return nullptr;
}

void *List_Emitter::user_label(Value tag)
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
    auto node = new Bytecode_List;
    node->is_label = true;
    node->is_internal_label = false;
    node->operands[0].num = m_label_id++;
    node->operands[1].value = tag;
    append(node);
    m_label_stack.back()[tag] = node;
    return node;
}

void *List_Emitter::internal_label(const char *label_tag)
{
    auto node = new Bytecode_List;
    node->is_label = true;
    node->is_internal_label = true;
    node->operands[0].num = m_label_id;
    node->operands[1].label_tag = label_tag;
    append(node);
    m_internal_labels[m_label_id] = node;
    m_label_id++;
    return node;
}

void *List_Emitter::emit_push_handler_case(uint32_t num_handlers)
{
    auto node = new Bytecode_List;
    node->opcode = Opcode::op_push_handler_case;
    node->operands[0].num = num_handlers;
    node->operands[1].destination = nullptr;
    append(node);
    return node;
}

void List_Emitter::close_push_handler_case(void *handler_case_id)
{
    auto cur = current();
    Bytecode_List *destination = nullptr;
    if (cur && cur->is_label)
    {
        destination = cur;
    }
    else
    {
        destination = reinterpret_cast<Bytecode_List*>(internal_label(nullptr));
    }
    auto hc = reinterpret_cast<Bytecode_List*>(handler_case_id);
    hc->operands[1].destination = destination;
}

Bytecode_List *List_Emitter::current() const
{
    return m_tail;
}

void List_Emitter::resolve_jump(void *branch_id, void *destination)
{
    auto br = reinterpret_cast<Bytecode_List*>(branch_id);
    br->operands[0].destination = reinterpret_cast<Bytecode_List*>(destination);
}

void List_Emitter::resolve_jump_to_current(void *branch_id)
{
    auto cur = current();
    Bytecode_List *destination = nullptr;
    if (cur && cur->is_label)
    {
        destination = cur;
    }
    else
    {
        destination = reinterpret_cast<Bytecode_List*>(internal_label(nullptr));
    }
    auto br = reinterpret_cast<Bytecode_List*>(branch_id);
    br->operands[0].destination = destination;
}

void List_Emitter::backfill_label(void *branch_id, Value tag)
{
    //auto offset = reinterpret_cast<intptr_t>(branch_id);
    //m_backfills.push_back({tag, static_cast<int32_t>(offset)});
    assert(m_label_stack.size() != 0);
    m_backfills.push_back({tag, reinterpret_cast<Bytecode_List*>(branch_id)});
}

bool List_Emitter::label_to_offset(void *label, uint32_t &out_offset)
{
    if (!m_finalized)
        return false;

    auto it = m_final_label_offset_map.find(label);
    if (it != m_final_label_offset_map.end())
    {
        out_offset = it->second;
        return true;
    }
    return false;
}

void List_Emitter::finalize()
{
    do_optimizations();

    convert_to_bytecode();
    
    m_finalized = true;
}

const std::vector<uint8_t> &List_Emitter::bytecode() const
{
    assert(m_finalized);
    return m_bytecode;
}

std::vector<uint8_t> &&List_Emitter::move_bytecode()
{
    assert(m_finalized);
    return std::move(m_bytecode);
}

Emitter *List_Emitter::of_same_type() const
{
    return new List_Emitter;
}

Bytecode_List *List_Emitter::head()
{
    auto cur = m_head;
    while (cur && cur->is_label)
    {
        cur = cur->next;
    }
    return cur;
}

Bytecode_List *List_Emitter::next(Bytecode_List *e)
{
    if (!e)
        return nullptr;

    return e->next;
    
    //e = e->next;
    //while (e && e->is_label)
    //{
    //    e = e->next;
    //}
    //return e;
}

Bytecode_List *List_Emitter::prev(Bytecode_List *e)
{
    if (!e)
        return nullptr;

    return e->prev;
    
    //e = e->prev;
    //while (e && e->is_label)
    //{
    //    e = e->prev;
    //}
    //return e;
}

Bytecode_List *List_Emitter::unlink(Bytecode_List *e)
{
    if (!e)
        return nullptr;

    if (e == m_head)
    {
        m_head = e->next;
        if (m_head)
            m_head->prev = nullptr;
    }
    if (e == m_tail)
    {
        m_tail = e->prev;
        if (m_tail)
            m_tail->next = nullptr;
    }
    else
    {
        // Notice that we do NOT set e->prev nor e->next so that calling prev(e) or next(e) will still work!
        if (e->prev)
            e->prev->next = e->next;

        if (e->next)
            e->next->prev = e->prev;

    }
    return e;
}

void List_Emitter::link(Bytecode_List *after, Bytecode_List *e)
{
    if (!after || !e)
        return;

    auto tmp = after->next;
    after->next = e;
    e->prev = after;

    auto tail = e;
    while (tail->next)
        tail = tail->next;

    tail->next = tmp;
    if (tmp)
        tmp->prev = tail;
}

static
bool is_push_insn(Bytecode_List *e)
{
    if (!e)
        return false;

    switch (e->opcode)
    {
        default:
            return false;

        case Opcode::op_push_value:
            return true;
    }
}

static
bool is_labeled(Bytecode_List *e)
{
    return e && e->prev && e->prev->is_label;
}

static
bool is_push_const_fixnum(Bytecode_List *e, Value &out_fixnum)
{
    if (!e)
        return false;

    if (e->opcode == Opcode::op_push_fixnum_0)
    {
        out_fixnum = Value::wrap_fixnum(0);
        return true;
    }
    if (e->opcode == Opcode::op_push_fixnum_1)
    {
        out_fixnum = Value::wrap_fixnum(1);
        return true;
    }
    if (e->opcode == Opcode::op_push_value)
    {
        out_fixnum = e->operands[0].value;
        return e->operands[0].value.is_fixnum();
    }

    return false;
}

static
bool is_push_constant(Bytecode_List *e, Value &out_value)
{
    if (!e)
        return false;

    if (e->opcode == Opcode::op_push_fixnum_0)
    {
        out_value = Value::wrap_fixnum(0);
        return true;
    }
    if (e->opcode == Opcode::op_push_fixnum_1)
    {
        out_value = Value::wrap_fixnum(1);
        return true;
    }
    if (e->opcode == Opcode::op_push_value)
    {
        out_value = e->operands[0].value;
        return true;
    }

    return false;
}

void List_Emitter::do_optimizations()
{
    static int total_unlinked_insns = 0;

    size_t unlinked_count = 0;
    std::vector<Bytecode_List*> unlinked;

    //std::cout << "Peephole optimization pass #0\n";
    //pp("");
    for (int i = 0; i < 10; ++i)
    {
        auto cur_insn = head();
        while (cur_insn)
        {
            auto prev_insn = prev(cur_insn);
            auto next_insn = next(cur_insn);

            /*
             * Remove instances of a pop preceded by some form of push.
             * e.g.
             *          push_value [5]
             *          pop
             *
             *          get_local [3]
             *          pop
             */
            #if OPT_REMOVE_NOOP_PUSH_POPS
            if (cur_insn->opcode == Opcode::op_pop)
            {
                if (is_push_insn(prev_insn))
                {
                    unlinked.push_back(unlink(prev_insn));
                    unlinked.push_back(unlink(cur_insn));
                }
            }
            #endif

            /*
             * Fold constants using op_add 
             * e.g.
             *          push_value [5]
             *          push_value [10]
             *          add
             *
             * to:      push_value [15]
             */
            #if OPT_FOLD_CONSTANT_ADDS
            if (cur_insn->opcode == Opcode::op_add)
            {
                auto b_insn = prev_insn;
                auto a_insn = prev(prev_insn);
                Value a, b;
                bool a_is_fixnum = is_push_const_fixnum(a_insn, a);
                bool b_is_fixnum = is_push_const_fixnum(b_insn, b);
                if (a_is_fixnum && b_is_fixnum)
                {
                    auto insn = new Bytecode_List;
                    insn->opcode = Opcode::op_push_value;
                    insn->operands[0].value = a + b;
                    link(cur_insn, insn);

                    unlinked.push_back(unlink(b_insn));
                    unlinked.push_back(unlink(a_insn));
                    unlinked.push_back(unlink(cur_insn));
                }
                else if (a_is_fixnum && a == Value::wrap_fixnum(1))
                {
                    auto insn = new Bytecode_List;
                    insn->opcode = Opcode::op_add_1;
                    link(cur_insn, insn);

                    unlinked.push_back(unlink(a_insn));
                    unlinked.push_back(unlink(cur_insn));
                }
                else if (b_is_fixnum && b == Value::wrap_fixnum(1))
                {
                    auto insn = new Bytecode_List;
                    insn->opcode = Opcode::op_add_1;
                    link(cur_insn, insn);

                    unlinked.push_back(unlink(b_insn));
                    unlinked.push_back(unlink(cur_insn));
                }
                else if (a_is_fixnum && a == Value::wrap_fixnum(0))
                {
                    unlinked.push_back(unlink(a_insn));
                    unlinked.push_back(unlink(cur_insn));
                }
                else if (b_is_fixnum && b == Value::wrap_fixnum(0))
                {
                    unlinked.push_back(unlink(b_insn));
                    unlinked.push_back(unlink(cur_insn));
                }
            }
            #endif
            /*
             * Fold constants using op_sub 
             * e.g.
             *          push_value [5]
             *          push_value [10]
             *          sub
             *
             * to:      push_value [15]
             */
            #if OPT_FOLD_CONSTANT_SUBS
            if (cur_insn->opcode == Opcode::op_sub)
            {
                auto b_insn = prev_insn;
                auto a_insn = prev(prev_insn);
                Value a, b;
                bool a_is_fixnum = is_push_const_fixnum(a_insn, a);
                bool b_is_fixnum = is_push_const_fixnum(b_insn, b);
                if (a_is_fixnum && b_is_fixnum)
                {
                    auto insn = new Bytecode_List;
                    insn->opcode = Opcode::op_push_value;
                    insn->operands[0].value = a - b;
                    link(cur_insn, insn);

                    unlinked.push_back(unlink(b_insn));
                    unlinked.push_back(unlink(a_insn));
                    unlinked.push_back(unlink(cur_insn));
                }
                else if (b_is_fixnum && b == Value::wrap_fixnum(1))
                {
                    auto insn = new Bytecode_List;
                    insn->opcode = Opcode::op_sub_1;
                    link(cur_insn, insn);

                    unlinked.push_back(unlink(b_insn));
                    unlinked.push_back(unlink(cur_insn));
                }
                else if (b_is_fixnum && b == Value::wrap_fixnum(0))
                {
                    unlinked.push_back(unlink(b_insn));
                    unlinked.push_back(unlink(cur_insn));
                }
            }
            #endif

            /* Convert sequences of get_X'ing the same thing into dup instructions
             * e.g.
             *            get_local 0
             *            get_local 0
             *            get_local 0
             * to:
             *            get_local 0
             *            dup
             *            dup
             */
            #if OPT_CONVERT_GET_SEQUENCES_TO_DUPS
            if (cur_insn->opcode == Opcode::op_get_local ||
                cur_insn->opcode == Opcode::op_get_global ||
                cur_insn->opcode == Opcode::op_get_capture)
            {
                auto opcode = cur_insn->opcode;
                auto idx = cur_insn->operands[0].num;
                auto nex = next_insn;
                while (nex && nex->opcode == opcode && nex->operands[0].num == idx)
                {
                    //auto insn = new Bytecode_List;
                    //insn->opcode = Opcode::op_dup;
                    //link(cur_insn, insn);
                    //unlinked.push_back(unlink(nex));
                    nex->opcode = Opcode::op_dup;
                    nex = next(nex);
                    total_unlinked_insns++;
                }
                cur_insn = nex;
                next_insn = next(cur_insn);
                prev_insn = prev(cur_insn);
            }
            #endif

            /*
             * Remove instances of jump instructions that directly to the next instruction
             * This should rarely ever be generated but may be a result of another
             * optimization.
             */
            if (cur_insn->opcode == Opcode::op_jump)
            {
                if (cur_insn->operands[0].destination == next_insn)
                {
                    unlinked.push_back(unlink(cur_insn));
                }
            }

            /*
             * Condense conditional branches with constants
             */
            if (cur_insn->opcode == Opcode::op_pop_jump_if_nil)
            {
                Value condition;
                if (is_push_constant(prev_insn, condition))
                {
                    if (condition == Value::nil())
                    {
                        auto dest = cur_insn->operands[0].destination;
                        auto cur = prev_insn;
                        while (cur && cur != dest)
                        {
                            unlinked.push_back(unlink(cur));
                            cur = cur->next;
                        }
                        cur_insn = dest;
                        continue;
                    }
                    else
                    {
                    }
                }
            }

            cur_insn = next_insn;
        }

        //std::cout << "Peephole optimization pass #" << i+1 << "\n";
        //pp("");

        if (unlinked.size() == unlinked_count)
            break; // no optimizations happened, just leave
        unlinked_count = unlinked.size();
    }
    total_unlinked_insns += unlinked.size();
    for (auto *p : unlinked)
    {
        delete p;
    }
    //std::cout << std::dec;
    //std::cout << "Peephole removed " << total_unlinked_insns << " instructions.\n";
}

static void pp_label(std::ostream &out, Bytecode_List *bc);
void List_Emitter::convert_to_bytecode()
{
    // We'll cheat and use the BC_Emitter to correctly generate the binary format
    BC_Emitter e;

    std::map<Bytecode_List*, void*>branches;
    
    auto cur = m_head;
    while (cur)
    {
        if (cur->is_label)
        {
            m_final_label_offset_map[cur] = e.position();
        }
        else switch (cur->opcode)
        {
            case Opcode::op_get_global:   e.emit_get_global(cur->operands[0].num); break;
            case Opcode::op_set_global:   e.emit_set_global(cur->operands[0].num); break;
            case Opcode::op_get_local:    e.emit_get_local(cur->operands[0].num); break;
            case Opcode::op_set_local:    e.emit_set_local(cur->operands[0].num); break;
            case Opcode::op_get_capture:  e.emit_get_capture(cur->operands[0].num); break;
            case Opcode::op_set_capture:  e.emit_set_capture(cur->operands[0].num); break;
            case Opcode::op_close_values: e.emit_close_values(cur->operands[0].num); break;
            case Opcode::op_raise_signal: e.emit_raise_signal(cur->operands[0].num); break;

            case Opcode::op_funcall:
                e.emit_funcall(cur->operands[0].num);
                break;
            case Opcode::op_gotocall:
                e.emit_gotocall(cur->operands[0].num);
                break;
            case Opcode::op_apply: e.emit_apply(cur->operands[0].num);
                break;

            case Opcode::op_jump:
                branches[cur] = e.emit_jump(nullptr);
                break;
            case Opcode::op_pop_jump_if_nil:
                branches[cur] = e.emit_pop_jump_if_nil(nullptr);
                break;
            
            case Opcode::op_function_value:
                e.emit_function_value(cur->operands[0].value);
                break;
            case Opcode::op_push_value:
                e.emit_push_value(cur->operands[0].value);
                break;

            case Opcode::op_push_handler_case:
                branches[cur] = e.emit_push_handler_case(cur->operands[0].num);
                break;

            case Opcode::op_instantiate_closure:
                e.emit_instantiate_closure(cur->operands[0].function);
                break;

            case Opcode::op_return:
                e.emit_return();
                break;
            case Opcode::op_pop:
                e.emit_pop();
                break;
            case Opcode::op_push_nil:
                e.emit_push_nil();
                break;
            case Opcode::op_push_fixnum_0:
                e.emit_push_fixnum_0();
                break;
            case Opcode::op_push_fixnum_1:
                e.emit_push_fixnum_1();
                break;
            case Opcode::op_cons:
                e.emit_cons();
                break;
            case Opcode::op_car:
                e.emit_car();
                break;
            case Opcode::op_cdr:
                e.emit_cdr();
                break;
            case Opcode::op_halt:
                e.emit_halt();
                break;
            case Opcode::op_pop_handler_case:
                e.emit_pop_handler_case();
                break;
            case Opcode::op_eq:
                e.emit_eq();
                break;
            case Opcode::op_rplaca:
                e.emit_rplaca();
                break;
            case Opcode::op_rplacd:
                e.emit_rplacd();
                break;
            case Opcode::op_aref:
                e.emit_aref();
                break;
            case Opcode::op_aset:
                e.emit_aset();
                break;
            case Opcode::op_debug_trap:
                e.emit_debug_trap();
                break;
            case Opcode::op_add:
                e.emit_add();
                break;
            case Opcode::op_add_1:
                e.emit_add_1();
                break;
            case Opcode::op_sub:
                e.emit_sub();
                break;
            case Opcode::op_sub_1:
                e.emit_sub_1();
                break;
            case Opcode::op_dup:
                e.emit_dup();
                break;
        }
        cur = cur->next;
    }

    for (auto [branch, e_branch_id] : branches)
    {
        if (branch->opcode == Opcode::op_push_handler_case)
        {
            auto dest_label = branch->operands[1].destination;
            assert(dest_label && dest_label->is_label);
            uintptr_t label_offset = m_final_label_offset_map[dest_label];

            e.close_push_handler_case(e_branch_id, reinterpret_cast<void*>(label_offset));
        }
        else if (branch->opcode == Opcode::op_jump || branch->opcode == Opcode::op_pop_jump_if_nil)
        {
            auto dest_label = branch->operands[0].destination;
            assert(dest_label && dest_label->is_label);
            uintptr_t label_offset = m_final_label_offset_map[dest_label];

            e.resolve_jump(e_branch_id, reinterpret_cast<void*>(label_offset));
        }
    }

    e.finalize();
    m_bytecode = std::move(e.move_bytecode());
}

static
void pp_label(std::ostream &out, Bytecode_List *bc)
{
    if (!bc)
    {
        out << "(nullptr)";
        return;
    }

    if (!bc->is_internal_label)
    {
        out << "L" << bc->operands[0].num << ":" << std::setw(35) << ";; ";
        auto obj_repr = repr(bc->operands[1].value);
        const int n = 25;
        if (obj_repr.size() < n) {
            out << "  [" << obj_repr << "]";
        }
        else {
            out << "  [" << obj_repr.substr(0, n-3) << "... ]";
        }
    }
    // is_internal is true
    else if (bc->operands[1].label_tag)
    {
        out << "L" << bc->operands[0].num << "_" << bc->operands[1].label_tag << ":";
    }
    // is_internal is true
    else
    {
        out << "L" << bc->operands[0].num << ":";
    }
}

void List_Emitter::pp(const char *tag)
{
    using namespace std;

    cout << tag << "\n";

    auto cur = m_head;
    while (cur)
    {
        const auto opc = cur->opcode;
        cout << setfill('0') << setw(8) << hex
             << reinterpret_cast<uintptr_t>(cur)
             << setfill(' ') << "  ";
        const auto name = opcode_name(opc);

        if (cur->is_label)
        {
            pp_label(cout, cur);
        }
        else switch (opc)
        {
            case Opcode::op_get_global:
            case Opcode::op_set_global:

            case Opcode::op_get_local:
            case Opcode::op_set_local:

            case Opcode::op_get_capture:
            case Opcode::op_set_capture:

            case Opcode::op_close_values:

            case Opcode::op_raise_signal:

            case Opcode::op_funcall:
            case Opcode::op_gotocall:
            case Opcode::op_apply:
            {
                cout << name << " " << cur->operands[0].num;
            } break;

            case Opcode::op_jump:
            case Opcode::op_pop_jump_if_nil:
            {
                auto dest = cur->operands[0].destination;
                cout << name << " -> ";
                pp_label(cout, dest);
                //if (dest && dest->is_label)
                //{
                //    cout << "LABEL_" << dest->operands[0].num << " @ ";
                //}
                //cout << setfill('0') << setw(8) << hex
                //     << reinterpret_cast<uintptr_t>(dest)
                //     << setfill(' ') << "  ";
            } break;
            
            case Opcode::op_function_value:
            case Opcode::op_push_value:
            {
                auto obj_repr = repr(cur->operands[0].value);
                const int n = 25;
                cout << name;
                if (obj_repr.size() < n) {
                    cout << "  [" << obj_repr << "]";
                }
                else {
                    cout << "  [" << obj_repr.substr(0, n-3) << "... ]";
                }
            } break;

            case Opcode::op_push_handler_case:
            {
                auto how_many = cur->operands[0].num;
                auto dest = cur->operands[1].destination;
                cout << name << " " << how_many << " -> "; 
                if (dest && dest->is_label)
                {
                    cout << "LABEL_" << dest->operands[0].num << " @ ";
                }
                cout << setfill('0') << setw(8) << hex
                     << reinterpret_cast<uintptr_t>(dest)
                     << setfill(' ') << "  ";
            } break;

            case Opcode::op_instantiate_closure:
            {
                auto function = cur->operands[0].function;
                cout << name << " " << reinterpret_cast<uintptr_t>(function);
                if (function)
                    cout << " -> {" << reinterpret_cast<uintptr_t>(function->begin()) << "}";
            } break;

            case Opcode::op_return:
            case Opcode::op_pop:
            case Opcode::op_push_nil:
            case Opcode::op_push_fixnum_0:
            case Opcode::op_push_fixnum_1:
            case Opcode::op_cons:
            case Opcode::op_car:
            case Opcode::op_cdr:
            case Opcode::op_halt:
            case Opcode::op_pop_handler_case:
            case Opcode::op_eq:
            case Opcode::op_rplaca:
            case Opcode::op_rplacd:
            case Opcode::op_aref:
            case Opcode::op_aset:
            case Opcode::op_debug_trap:
            case Opcode::op_add:
            case Opcode::op_add_1:
            case Opcode::op_sub:
            case Opcode::op_sub_1:
            case Opcode::op_dup:
            {
                cout << name;
            } break;
        }
        cout << "\n";
        cur = cur->next;
    }
}

void List_Emitter::do_tests()
{
    push_labels();
    emit_push_nil();

    auto here = internal_label(nullptr);
    emit_push_fixnum_0();
    emit_push_fixnum_1();
    emit_jump(here);

    auto jmp1 = emit_pop_jump_if_nil(nullptr);
    auto jmp2 = emit_pop_jump_if_nil(nullptr);
    auto jmp3 = emit_pop_jump_if_nil(nullptr);

    auto u_label = user_label(g.s_CONS);

    emit_funcall(1);
    emit_gotocall(2);
    emit_apply(3);

    emit_jump(u_label);

    resolve_jump_to_current(jmp1);
    resolve_jump_to_current(jmp2);
    resolve_jump_to_current(jmp3);
    
    pp("do_tests");

}
