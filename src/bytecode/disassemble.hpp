#ifndef _LISPYBOI_BYTECODE_DISASSEMBLE_
#define _LISPYBOI_BYTECODE_DISASSEMBLE_

#include <iostream>
#include <string>

#include "../defines.hpp"
#include "../value.hpp"

namespace lisp::bytecode
{
struct Emitter;

void put_bytes(std::ostream &out, const uint8_t *bytes, size_t nbytes, size_t min_width=10*3);

const uint8_t *disassemble1(std::ostream &out, const uint8_t *ip, bool here);

int put_disassembly_tag(std::ostream &out, const std::string &tag);

const Symbol *find_symbol_with_function(const Function *function);
const Symbol *find_symbol_with_function(const uint8_t *ip);

int disassemble(std::ostream &out, const std::string &tag, const uint8_t *ip, bool here = false);

int disassemble(std::ostream &out, const std::string &tag, const uint8_t *start, const uint8_t *end, const uint8_t *ip = nullptr);

int disassemble(std::ostream &out, const std::string &tag, const Emitter &e);

int disassemble(std::ostream &out, const std::string &tag, const Function *function, const uint8_t *ip);

int disassemble_maybe_function(std::ostream &out, std::string tag, const uint8_t *ip, bool here = false);

}
#endif
