#include <iomanip>

#include "../lisp_reference_types.hpp"
#include "../runtime_globals.hpp"
#include "../util.hpp"
#include "opcode.hpp"
#include "emitter.hpp"
#include "compiler.hpp"
#include "disassemble.hpp"

namespace lisp::bytecode
{
void put_bytes(std::ostream &out, const uint8_t *bytes, size_t nbytes, size_t min_width/*=10*3*/)
{
    size_t column = 0;
    for (size_t i = 0; i < nbytes; ++i) {
        out << std::setfill('0') << std::setw(2) << std::hex << (int)bytes[i] << ' ';
        column += 3;
    }

    for (; column < min_width; ++column) {
        out << ' ';
    }
}

const uint8_t *disassemble1(std::ostream &out, const uint8_t *ip, bool here)
{
    const auto opcode = static_cast<Opcode>(*ip);
    if (here) {
        out << ">> ";
    }
    else {
        out << "   ";
    }
    out << std::setfill('0') << std::setw(8) << std::hex << reinterpret_cast<uintptr_t>(ip) << std::setfill(' ') << "  ";
    auto size = opcode_size(opcode);
    auto name = opcode_name(opcode);
    switch (opcode) {
        default:
        {
            put_bytes(out, ip, size);
            out << name;
            ip += size;
        } break;

        case Opcode::op_get_global:
        case Opcode::op_set_global:

        case Opcode::op_get_local:
        case Opcode::op_set_local:

        case Opcode::op_get_capture:
        case Opcode::op_set_capture:

        case Opcode::op_close_values:

        case Opcode::op_raise_signal:

        case Opcode::op_apply:
        case Opcode::op_funcall:
        case Opcode::op_gotocall:
        {
            auto nargs = *reinterpret_cast<const uint32_t*>(ip+1);
            put_bytes(out, ip, size);
            out << name << " " << nargs;
            ip += size;
        } break;


        case Opcode::op_jump:
        case Opcode::op_pop_jump_if_nil:
        {
            auto offs = *reinterpret_cast<const int32_t*>(ip+1);
            put_bytes(out, ip, size);
            out << name << " " << offs << " -> " << reinterpret_cast<uintptr_t>(ip+offs);
            ip += size;
        } break;

        case Opcode::op_function_value:
        case Opcode::op_push_value:
        {
            auto obj = *reinterpret_cast<const Value*>(ip+1);
            put_bytes(out, ip, size);
            out << name << " " << obj.bits();
            auto obj_repr = repr(obj);
            const int n = 25;
            if (obj_repr.size() < n) {
                out << "  [" << obj_repr << "]";
            }
            else {
                out << "  [" << obj_repr.substr(0, n-3) << "... ]";
            }
            ip += size;
        } break;

        case Opcode::op_push_handler_case:
        {
            put_bytes(out, ip, size);
            auto how_many = *reinterpret_cast<const uint32_t*>(ip+1);
            auto branch = *reinterpret_cast<const uint32_t*>(ip+1+sizeof(how_many));
            out << name << " " << how_many << ", " << branch
                << " -> " << reinterpret_cast<uintptr_t>(ip+branch);
            ip += size;
        } break;

        case Opcode::op_instantiate_closure:
        {
            auto function = *reinterpret_cast<const Function* const*>(ip+1);
            put_bytes(out, ip, size);
            out << name << " " << reinterpret_cast<uintptr_t>(function)
                << " -> {" << reinterpret_cast<uintptr_t>(function->begin()) << "}";
            ip += size;
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
            put_bytes(out, ip, size);
            out << name;
            ip += size;
        } break;
    }
    out << '\n';
    return ip;
}

int put_disassembly_tag(std::ostream &out, const std::string &tag)
{
    out << "Disassembly for \"" << tag << "\"\n";
    return 1;
}

const Symbol *find_symbol_with_function(const Function *function)
{
    if (!function)
        return nullptr;

    for (const auto &[_, package] : g.packages.packages())
    {
        if (!package)
            continue;
        for (const auto &[_, value] : package->symbols())
        {
            if (!symbolp(value))
                continue;
            const auto symbol = value.as_object()->symbol();
            if (!symbol)
                continue;
            if (symbol->package() != package)
                continue;
            if (!symbol->has_function())
                continue;
            
            auto func_value = symbol->function();
            if (!func_value.is_object())
                continue;
            
            const auto func_obj = func_value.as_object();
            if (func_obj->type() != Object_Type::Closure)
                continue;

            const auto closure = func_obj->closure();

            if (closure && closure->function() == function)
            {
                return symbol;
            }
        }
    }
    return nullptr;
}

int disassemble(std::ostream &out, const std::string &tag, const uint8_t *ip, bool here/* = false*/)
{
    put_disassembly_tag(out, tag);
    disassemble1(out, ip, here);
    return 2;
}

int disassemble(std::ostream &out, const std::string &tag, const uint8_t *start, const uint8_t *end, const uint8_t *ip/* = nullptr*/)
{
    int lines_printed = put_disassembly_tag(out, tag);
    for (; start != end;) {
        start = disassemble1(out, start, start == ip);
        lines_printed++;
    }
    return lines_printed;
}

int disassemble(std::ostream &out, const std::string &tag, const Emitter &e)
{
    auto start = e.bytecode().data();
    auto end = start + e.bytecode().size();
    return disassemble(out, tag, start, end, nullptr);
}

int disassemble(std::ostream &out, const std::string &tag, const Function *function, const uint8_t *ip)
{
    int lines_printed = put_disassembly_tag(out, tag);
    for (auto p = function->begin(); p != function->end();)
    {
        p = disassemble1(out, p, p == ip);
        lines_printed++;
    }
    return lines_printed;
}

int disassemble_maybe_function(std::ostream &out, std::string tag, const uint8_t *ip, bool here/* = false*/)
{
    const Function *func;
    if (Debug_Info::find_function(ip, &func))
    {
        tag = "<Anonymous Closure>";
        auto symbol = find_symbol_with_function(func);
        if (symbol != nullptr)
        {
            tag = symbol->qualified_name();
        }
        return disassemble(out, tag, func, ip);
    }

    put_disassembly_tag(out, tag);
    disassemble1(out, ip, here);
    return 2;
}

}
