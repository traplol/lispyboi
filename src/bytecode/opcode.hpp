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

static
std::string opcode_name(Opcode opcode)
{
    switch (opcode)
    {
#define BYTECODE_DEF(name, noperands, nargs, size, docstring) case Opcode::op_ ## name: return #name;
#include "../bytecode.def"
#undef BYTECODE_DEF
    }
    return "???";
}

static
size_t opcode_size(Opcode opcode)
{
    switch (opcode)
    {
#define BYTECODE_DEF(name, noperands, nargs, size, docstring) case Opcode::op_ ## name: return size;
#include "../bytecode.def"
#undef BYTECODE_DEF
    }
    return 1;
}
}
}

#endif
