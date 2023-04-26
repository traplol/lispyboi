#include <iostream>
#include <iomanip>
#include "../defines.hpp"
#include "../vm_state.hpp"
#include "list_emitter.hpp"

using namespace lisp;
using namespace lisp::bytecode;

void List_Emitter::append(Bytecode_List *node)
{
    if (m_head == nullptr)
    {
        m_head = node;
        m_current = m_head;
    }
    else if (node != nullptr)
    {
        m_current->next = node;
        node->prev = m_current;
        m_current = node;
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
    m_label_stack.pop_back();
}

void *List_Emitter::get_label(Value tag)
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
    node->operands[0].num = m_label_id++;
    append(node);
    m_label_stack.back()[tag] = node;
    return node;
}

void *List_Emitter::internal_label()
{
    auto node = new Bytecode_List;
    node->is_label = true;
    node->operands[0].num = m_label_id;
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
        destination = reinterpret_cast<Bytecode_List*>(internal_label());
    }
    auto hc = reinterpret_cast<Bytecode_List*>(handler_case_id);
    hc->operands[1].destination = destination;
}

Bytecode_List *List_Emitter::current() const
{
    return m_current;
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
        destination = reinterpret_cast<Bytecode_List*>(internal_label());
    }
    auto br = reinterpret_cast<Bytecode_List*>(branch_id);
    br->operands[0].destination = destination;
}

void List_Emitter::backfill_label(void *branch_id, Value tag)
{
    //auto offset = reinterpret_cast<intptr_t>(branch_id);
    //assert(m_label_stack.size() != 0);
    //m_backfills.push_back({tag, static_cast<int32_t>(offset)});
}


void List_Emitter::pp()
{
    using namespace std;

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
            cout << "LABEL_" << cur->operands[0].num << ":";
        }
        else switch (opc)
        {
            default: cout << "pp not implemented for op_" << name; break;

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
                if (dest && dest->is_label)
                {
                    cout << "LABEL_" << dest->operands[0].num << " @ ";
                }
                cout << setfill('0') << setw(8) << hex
                     << reinterpret_cast<uintptr_t>(dest)
                     << setfill(' ') << "  ";
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

    auto here = internal_label();
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
    
    pp();

}
