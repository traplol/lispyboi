#ifndef _LISPYBOI_BYTECODE_OPCODE_
#define _LISPYBOI_BYTECODE_OPCODE_

#include "../defines.hpp"
#include "../value.hpp"

namespace lisp
{
namespace bytecode
{
enum class Opcode : uint8_t
{
#define BYTECODE_DEF(name, noperands, nargs, size, docstring) op_ ## name,
#include "../bytecode.def"
#undef BYTECODE_DEF
};

std::string opcode_name(Opcode opcode);
size_t opcode_noperands(Opcode opcode);
size_t opcode_nargs(Opcode opcode);
size_t opcode_size(Opcode opcode);
std::string opcode_docstring(Opcode opcode);

}
}

#endif
