#include <iomanip>

#include "util.hpp"
#include "bytecode/opcode.hpp"
#include "bytecode/bc_emitter.hpp"
#include "bytecode/compiler.hpp"
#include "bytecode/disassemble.hpp"
#include "vm_state.hpp"

using namespace lisp;

VM_State *lisp::THE_LISP_VM;

int VM_State::stack_dump(std::ostream &out, size_t max_size) const
{
    if (max_size == ~0u)
    {
        auto a = (m_call_frame_top - m_call_frame_bottom);
        auto b = (m_stack_top - m_stack_bottom);
        max_size = std::max(a, b);
    }
    auto rt = m_call_frame_top;
    auto rb = m_call_frame_bottom;

    auto pt = m_stack_top;
    auto pb = m_stack_bottom;

    auto r_stack_delta = rt - rb;
    out << std::setfill(' ') << std::dec;
    out << "|R-stack depth=" << r_stack_delta << '\n';
    out << "|===================================\n";
    out << std::setfill('0');

    for (size_t i = max_size; i != 0; --i)
    {
        rt--;
        if (reinterpret_cast<uintptr_t>(rt) < reinterpret_cast<uintptr_t>(rb))
        {
            out << "| ****************  ";
        }
        else
        {
            out << "| " << std::hex << std::setw(16) << reinterpret_cast<uintptr_t>(rt->ip) << "  ";
            if (auto symbol = bytecode::find_symbol_with_function(rt->ip))
            {
                out << symbol->qualified_name();
            }
            else
            {
                out << "<Anonymous Closure>";
            }
        }
        out << '\n';
    }

    out << std::setfill(' ') << std::dec;
    out << "|P-stack \n";
    out << "|===================================\n";
    out << std::setfill('0');

    for (size_t i = max_size; i != 0; --i)
    {
        pt--;

        if (reinterpret_cast<uintptr_t>(pt) < reinterpret_cast<uintptr_t>(pb))
        {
            out << "|                ";
        }
        else
        {
            out << "| " << std::hex << std::setw(16) << reinterpret_cast<uintptr_t>(pt);
        }
        if (pt == m_locals)
        {
            out << " -> ";
        }
        else
        {
            out << "    ";
        }

        if (reinterpret_cast<uintptr_t>(pt) < reinterpret_cast<uintptr_t>(pb))
        {
            out << " ***";
        }
        else
        {
            auto obj_repr = repr(*pt);
            const int n = 70;
            if (obj_repr.size() < n)
            {
                out << " " << obj_repr;
            }
            else
            {
                out << " " << obj_repr.substr(0, n-3) << "...";
            }
        }
        out << "\n";
    }
    int lines_printed = max_size;
    if (!m_current_closure.is_nil())
    {
        auto cc = m_current_closure.as_object()->closure();
        auto func_caps = cc->function()->capture_offsets();
        out << "Captures for current closure:\n";
        lines_printed++;
        auto &caps = cc->captures();
        for (size_t i = 0; i < caps.size(); ++i)
        {
            auto &offs = func_caps[i];
            out << offs.name << " "
                << std::hex << reinterpret_cast<uintptr_t>(caps[i]->location()) << " "
                << (caps[i] ? repr(caps[i]->value()) : "#<nullptr>") << "\n";
            lines_printed++;
        }
    }
    return lines_printed;
}

void VM_State::debug_dump(std::ostream &out, std::string tag, const uint8_t *ip, bool full) const
{
    //plat::clear_console();
    const Function *function;
    int lines_printed = 0;
    if (bytecode::Debug_Info::find_function(ip, &function))
    {
        if (auto symbol = bytecode::find_symbol_with_function(ip))
        {
            tag = symbol->qualified_name();
        }

        std::vector<const uint8_t*> instructions;
        int ip_at = -1;
        for (auto it = function->begin(); it != function->end();)
        {
            if (it == ip)
            {
                ip_at = instructions.size();
            }
            instructions.push_back(it);
            it += bytecode::opcode_size(static_cast<bytecode::Opcode>(*it));
        }
        instructions.push_back(function->end());

        if (ip_at != -1 && instructions.size() >= 32)
        {
            out << "Disassemble for \"" << tag << "\"\n";
            lines_printed++;

            auto first = std::max(ip_at - 15, 0);
            auto instr = instructions[first];
            bool disassembling = true;
            for (; lines_printed < 32; ++lines_printed)
            {
                if (instr == function->end())
                {
                    disassembling = false;
                }

                if (disassembling)
                {
                    instr = bytecode::disassemble1(out, instr, instr == ip);
                }
                else
                {
                    out << "\n";
                }
            }
        }
        else
        {
            lines_printed = bytecode::disassemble(out, tag, function, ip);
        }
    }
    else
    {
        lines_printed = bytecode::disassemble(out, tag, ip, true);
    }

    //for (; lines_printed <= 32; lines_printed++)
    //{
    //    out << "\n";
    //}

    if (full)
    {
        stack_dump(out, ~0u);
    }
    else
    {
        stack_dump(out);
    }
}

Value VM_State::alloc_signal_context(Value signal_args, const uint8_t *ip)
{
    Value ctx;
    std::vector<const uint8_t*> call_trace;
    call_trace.push_back(ip);
    for (auto p = m_call_frame_top; p != m_call_frame_bottom; --p)
    {
        call_trace.push_back(p->ip);
    }
    ctx = gc.alloc_object<Signal_Context>(first(signal_args), ip, signal_args, call_trace);
    return ctx;
}

bool VM_State::find_handler(Value tag, bool auto_pop, Handler_Case &out_case_state, Signal_Handler &out_handler)
{
    bool found = false;
    size_t npop = 0;
    for (auto it = m_handler_cases.rbegin(); !found && it != m_handler_cases.rend(); it++)
    {
        npop++;
        auto &handlers = it->handlers;
        for (auto h = handlers.rbegin(); h != handlers.rend(); ++h)
        {
            if (h->tag == g.s_T || h->tag == tag)
            {
                out_case_state = *it;
                out_handler = *h;
                found = true;
                break;
            }
        }
    }

    if (auto_pop)
    {
        m_handler_cases.resize(m_handler_cases.size() - npop);
    }
    return found;
}

Value VM_State::call_lisp_function(Value function_or_symbol, Value *args, uint32_t nargs)
{
    auto function = symbolp(function_or_symbol)
        ? function_or_symbol.as_object()->symbol()->function()
        : function_or_symbol;

    // What better way to reduce code sync bugs than by just letting the VM handle dispatching the call?
    if (m_stub.emitter == nullptr)
    {
        m_stub.emitter = new bytecode::BC_Emitter;
        m_stub.function_offset = m_stub.emitter->position() + 1;
        m_stub.emitter->emit_push_value(function);
        m_stub.nargs_offset = m_stub.emitter->position() + 1;
        m_stub.emitter->emit_funcall(nargs);
        m_stub.emitter->emit_halt();
        m_stub.emitter->lock();
    }
    else
    {
        m_stub.emitter->set_raw(m_stub.function_offset, function);
        m_stub.emitter->set_raw(m_stub.nargs_offset, nargs);
    }
    for (uint32_t i = 0; i < nargs; ++i)
    {
        push_param(args[i]);
    }
    execute(m_stub.emitter->bytecode().data());
    return pop_param();
}
