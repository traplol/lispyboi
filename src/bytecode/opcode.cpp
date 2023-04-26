#include "opcode.hpp"

using namespace lisp::bytecode;

std::string lisp::bytecode::opcode_name(Opcode opcode)
{
    switch (opcode)
    {
#define BYTECODE_DEF(name, noperands, nargs, size, docstring) case Opcode::op_ ## name: return #name;
#include "../bytecode.def"
#undef BYTECODE_DEF
    }
    return "???";
}

size_t lisp::bytecode::opcode_size(Opcode opcode)
{
    switch (opcode)
    {
#define BYTECODE_DEF(name, noperands, nargs, size, docstring) case Opcode::op_ ## name: return size;
#include "../bytecode.def"
#undef BYTECODE_DEF
    }
    return 1;
}

size_t lisp::bytecode::opcode_noperands(Opcode opcode)
{
    switch (opcode)
    {
#define BYTECODE_DEF(name, noperands, nargs, size, docstring) case Opcode::op_ ## name: return noperands;
#include "../bytecode.def"
#undef BYTECODE_DEF
    }
    return 0;
}

size_t lisp::bytecode::opcode_nargs(Opcode opcode)
{
    switch (opcode)
    {
#define BYTECODE_DEF(name, noperands, nargs, size, docstring) case Opcode::op_ ## name: return nargs;
#include "../bytecode.def"
#undef BYTECODE_DEF
    }
    return 0;
}

std::string lisp::bytecode::opcode_docstring(Opcode opcode)
{
    switch (opcode)
    {
#define BYTECODE_DEF(name, noperands, nargs, size, docstring) case Opcode::op_ ## name: return docstring;
#include "../bytecode.def"
#undef BYTECODE_DEF
    }
    return "Unknown opcode";
}
