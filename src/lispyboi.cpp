#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <string>
#include <vector>
#include <list>
#include <array>
#include <unordered_map>
#include <unordered_set>
#include <functional>
#include <algorithm>
#include <chrono>

#include <filesystem>
#include <sstream>
#include <fstream>

#include "defines.hpp"
#include "value.hpp"
#include "ffi.hpp"
#include "platform.hpp"
#include "runtime_globals.hpp"
#include "util.hpp"
#include "bytecode/compiler.hpp"
#include "bytecode/emitter.hpp"
#include "bytecode/opcode.hpp"
#include "bytecode/disassemble.hpp"

lisp::compiler::Scope *THE_ROOT_SCOPE;


namespace lisp
{

GC gc;
Runtime_Globals g;

static FORCE_INLINE
bool only_fixnums(const Value *args, uint32_t nargs)
{
    // This also returns true for an empty list.
    for (uint32_t i = 0; i < nargs; ++i)
    {
        if (!args[i].is_fixnum())
        {
            return false;
        }
    }
    return true;
}

bool stringp(Value v);

std::string Symbol::qualified_name() const
{
    std::string res;
    if (m_package)
    {
        if (is_keyword())
        {
            res += ":";
        }
        else {
            res += m_package->name();
            if (m_package->is_exported(m_name))
            {
                res += ":";
            }
            else
            {
                res += "::";
            }
        }
        res += m_name;
    }
    else
    {
        res += "#:";
        res += m_name;
    }
    return res;
}


namespace bytecode
{
struct Emitter;
}

struct VM_State
{
    struct Call_Frame
    {
        const uint8_t *ip;
        Value current_closure;
        Value *locals;
        Value *stack_top;
    };

    struct Exception
    {
        Exception(Value what) : what(what) {}

        Value what;
    };

    struct Unhandleable_Exception : Exception
    {
        const char *msg;
    };

    struct Signal_Exception : Exception
    {
        Signal_Exception(Value what)
            : Exception(what)
            , ip(nullptr)
            , stack_trace_top(nullptr)
            , stack_trace_bottom(nullptr)
        {}

        Signal_Exception(Value what, const uint8_t *ip, Call_Frame *stack_top, Call_Frame *stack_bottom)
            : Exception(what)
            , ip(ip)
            , stack_trace_top(stack_top)
            , stack_trace_bottom(stack_bottom)
        {}

        const uint8_t *ip;
        const Call_Frame *stack_trace_top;
        const Call_Frame *stack_trace_bottom;
    };


    VM_State()
        : m_current_closure(Value::nil())
        , m_open_closure_references(nullptr)
    {
        m_locals = m_stack_top = m_stack_bottom = new Value[0x100000];
        m_call_frame_top = m_call_frame_bottom = new Call_Frame[0x10000];
        gc.register_marking_function([this](GC &gc) { gc_mark(gc); });
    }

    Value &param_top()
    {
        return *(m_stack_top - 1);
    }

    Value &param_top(int32_t n)
    {
        return *(m_stack_top - 1 + n);
    }

    void push_param(Value v)
    {
        *m_stack_top++ = v;
    }

    Value pop_param()
    {
        return *--m_stack_top;
    }

    void pop_params(uint32_t n)
    {
        m_stack_top -= n;
    }

    void push_frame(const uint8_t *ip, uint32_t nargs)
    {
        Call_Frame frame {
            ip,
            m_current_closure,
            m_locals,
            m_stack_top - nargs
        };
        *m_call_frame_top++ = frame;
    }

    Call_Frame pop_frame()
    {
        return *--m_call_frame_top;
    }

    Call_Frame &frame_top()
    {
        return *(m_call_frame_top - 1);
    }

    void set_frame(const Call_Frame &frame)
    {
        m_current_closure = frame.current_closure;
        m_locals = frame.locals;
        m_stack_top = frame.stack_top;
    }

    Call_Frame &get_frame(uint32_t idx)
    {
        return *(m_call_frame_top - idx);
    }

    const Value *stack_top() const
    {
        return m_stack_top;
    }

    const Value *stack_bottom() const
    {
        return m_stack_bottom;
    }

    const Call_Frame *call_frame_top() const
    {
        return m_call_frame_top;
    }

    const Call_Frame *call_frame_bottom() const
    {
        return m_call_frame_bottom;
    }


    template<bool debuggable>
    const uint8_t *execute_impl(const uint8_t *ip);

    inline const uint8_t *execute(const uint8_t *ip);

    Value call_lisp_function(Value function_or_symbol, Value *args, uint32_t nargs);

    void debug_dump(std::ostream &out, const std::string &tag, const uint8_t *ip, bool full = false) const;
    int stack_dump(std::ostream &out, size_t max_size = 15) const;

    struct Signal_Handler
    {
        Value tag;
        Value handler;
    };

    struct Handler_Case
    {
        Value *stack;
        Call_Frame *frame;
        std::vector<Signal_Handler> handlers;
    };

    struct Save_State
    {
        Value current_closure;
        Value *locals;
        Value *stack_top, *stack_bottom;
        Call_Frame *call_frame_top, *call_frame_bottom;
        Closure_Reference *open_closure_references;
        std::vector<Handler_Case> handler_cases;
    };

    Save_State save()
    {
        return {
            m_current_closure,
            m_locals,
            m_stack_top, m_stack_bottom,
            m_call_frame_top, m_call_frame_bottom,
            m_open_closure_references,
            m_handler_cases
        };
    }

    void restore(Save_State &state)
    {
        m_current_closure = state.current_closure;
        m_locals = state.locals;
        m_stack_top = state.stack_top;
        m_stack_bottom = state.stack_bottom;
        m_call_frame_top = state.call_frame_top;
        m_call_frame_bottom = state.call_frame_bottom;
        m_open_closure_references = state.open_closure_references;
        m_handler_cases = state.handler_cases;
    }

    __attribute__((used))
    __attribute__((noinline))
    void ez_debug(const uint8_t *ip)
    {
        debug_dump(std::cout, "EZ DEBUG", ip, true);
    }

    #if PROFILE_OPCODE_PAIRS
    const std::unordered_map<Opcode_Pair, int> &opcode_pairs() const
    {
        return m_opcode_pairs;
    }
    #endif

  private:

    void push_handler_case(std::vector<Signal_Handler> &&handlers)
    {
        m_handler_cases.push_back({m_stack_top, m_call_frame_top, std::move(handlers)});
    }

    void pop_handler_case()
    {
        m_handler_cases.pop_back();
    }

    bool find_handler(Value tag, bool auto_pop, Handler_Case &out_case_state, Signal_Handler &out_handler);

    void gc_mark(GC &gc)
    {
        for (auto p = m_stack_bottom; p != m_stack_top; ++p)
        {
            gc.mark_value(*p);
        }

        // Ensure our stack of closures don't accidently get GC
        for (auto p = m_call_frame_bottom; p != m_call_frame_top; ++p)
        {
            gc.mark_value(p->current_closure);
        }

        gc.mark_value(m_current_closure);

        gc.mark_closure_reference(m_open_closure_references);

        for (auto &handler_case : m_handler_cases)
        {
            for (auto &handler : handler_case.handlers)
            {
                gc.mark_value(handler.tag);
                gc.mark_value(handler.handler);
            }
        }
    }

    Closure_Reference *capture_closure_reference(Value *local)
    {
        Closure_Reference* prev_ref = nullptr;
        auto curr_ref = m_open_closure_references;

        while (curr_ref != nullptr && std::greater()(curr_ref->location(), local))
        {
            prev_ref = curr_ref;
            curr_ref = curr_ref->next;
        }

        if (curr_ref != nullptr && curr_ref->location() == local) return curr_ref;

        auto new_ref = gc.make_closure_reference(local);
        new_ref->next = curr_ref;

        if (prev_ref == nullptr)
        {
            m_open_closure_references = new_ref;
        }
        else
        {
            prev_ref->next = new_ref;
        }

        return new_ref;
    }

    void close_values(Value *end)
    {
        while (m_open_closure_references != nullptr
               && m_open_closure_references->location() >= end)
        {
            m_open_closure_references->close();
            m_open_closure_references = m_open_closure_references->next;
        }
    }


    Value m_current_closure;
    Value *m_locals;
    Value *m_stack_top, *m_stack_bottom;
    Call_Frame *m_call_frame_top, *m_call_frame_bottom;
    Closure_Reference *m_open_closure_references;
    std::vector<Handler_Case> m_handler_cases;

    struct Call_Lisp_From_Native_Stub
    {
        Call_Lisp_From_Native_Stub()
            : emitter(nullptr)
            , nargs_offset(0)
            , function_offset(0)
        {}
        bytecode::Emitter *emitter;
        uint32_t nargs_offset;
        uint32_t function_offset;
    } m_stub;

    #if PROFILE_OPCODE_PAIRS
    std::unordered_map<Opcode_Pair, int> m_opcode_pairs;
    #endif

};
static VM_State *THE_LISP_VM;
static Value macro_expand_impl(Value obj, VM_State &vm, bool just_one = false);

inline const uint8_t *VM_State::execute(const uint8_t *ip)
{
    #if USE_COMPUTED_GOTOS
    if (g.debugger.breaking)
    {
        return execute_impl<true>(ip);
    }
    return execute_impl<false>(ip);
    #else
    return execute_impl<true>(ip);
    #endif
}

std::string repr(Value value)
{
    if (value.is_fixnum())
    {
        return std::to_string(value.as_fixnum());
    }

    if (value.is_nil())
    {
        return "NIL";
    }

    if (value.is_cons())
    {
        std::string res = "(";
        res += repr(car(value));
        value = cdr(value);
        while (value.is_cons() && !value.is_nil())
        {
            res += " ";
            res += repr(car(value));
            value = cdr(value);
        }
        if (!value.is_cons() && !value.is_nil())
        {
            res += " . ";
            res += repr(value);
        }
        res += ")";
        return res;
    }

    if (value.is_character())
    {
        auto codepoint = value.as_character();
        switch (codepoint)
        {
            default:   return std::string("#\\") + reinterpret_cast<const char*>(&codepoint);
            case ' ':  return "#\\Space";
            case '\t': return "#\\Tab";
            case '\n': return "#\\Newline";
            case '\r': return "#\\Return";
        }
    }

    if (value.is_object())
    {
        auto obj = value.as_object();
        switch (obj->type())
        {
            case Object_Type::Symbol:
            {
                auto symbol = obj->symbol();
                //return symbol->qualified_name();
                std::string str;
                if (symbol->is_keyword())
                {
                    str += ":";
                }
                else if (symbol->package() == nullptr)
                {
                    str += "#:";
                }
                return str + obj->symbol()->name();
            }
            case Object_Type::Closure:
            {
                std::stringstream ss;
                auto clos = obj->closure();
                ss << "#<LAMBDA " << std::hex << reinterpret_cast<uintptr_t>(clos)
                   << " -> {" << reinterpret_cast<uintptr_t>(clos->function()->begin()) << "}>";
                return ss.str();
            }
            case Object_Type::Package:
            {
                std::string res;
                res += "#<PACKAGE ";
                res += obj->package()->name();
                res += ">";
                return res;
            }
            case Object_Type::File_Stream: return "#<FILE-STREAM>";
            case Object_Type::Simple_Array:
            {
                auto array = obj->simple_array();
                if (array->element_type() == g.s_CHARACTER)
                {
                    auto str = lisp_string_to_native_string(value);
                    std::string res;
                    res += "\"";
                    for (auto c : str)
                    {
                        switch (c)
                        {
                            default:
                                res += c;
                                break;
                            case '"':
                                res += "\\\"";
                                break;
                            case '\r':
                                res += "\\r";
                                break;
                            case '\t':
                                res += "\\t";
                                break;
                            case '\n':
                                res += "\\n";
                                break;
                        }
                    }
                    res += "\"";
                    return res;
                }
                else
                {
                    std::string res;
                    res += "#(";
                    if (array->size() > 0)
                    {
                        res += repr(array->at(0));
                    }
                    for (Fixnum i = 1; i < array->size(); ++i)
                    {
                        res += " ";
                        res += repr(array->at(i));
                    }
                    res += ")";
                    return res;
                }
            }
            case Object_Type::System_Pointer:
            {
                std::stringstream ss;
                ss << "#<SYSTEM-POINTER 0x"
                   << std::hex << reinterpret_cast<uintptr_t>(obj->system_pointer())
                   << ">";
                return ss.str();
            }
            case Object_Type::Structure: return "#<STRUCTURE>";
            case Object_Type::Float:
            {
                return std::to_string(obj->to_float());
            }
        }
    }

    if (value.is_lisp_primitive())
    {
        return "#<PRIMITIVE>";
    }
    return "#<!REPR WTF!>";
}

bool Symbol::is_keyword() const
{
    return m_package == g.keyword();
}

FORCE_INLINE
bool stringp(Value v)
{
    return v.is_type(Object_Type::Simple_Array) &&
        v.as_object()->simple_array()->element_type() == g.s_CHARACTER;
}


Value GC::alloc_string(const char *str, Fixnum len)
{
    std::vector<uint32_t> codepoints;
    // valid utf-8 codepoint enumeration
    for(Fixnum i = 0; i < len;)
    {
        int cp_len = 1;
        if ((str[i] & 0xf8) == 0xf0)
        {
            cp_len = 4;
        }
        else if ((str[i] & 0xf0) == 0xe0)
        {
            cp_len = 3;
        }
        else if ((str[i] & 0xe0) == 0xc0)
        {
            cp_len = 2;
        }
        if ((i + cp_len) > len)
        {
            cp_len = 1;
        }

        int32_t codepoint = 0;
        switch (cp_len)
        {
            // neat use of a fallthrough.
            case 4: codepoint |= (str[i+3] & 0xff) << 24;
            case 3: codepoint |= (str[i+2] & 0xff) << 16;
            case 2: codepoint |= (str[i+1] & 0xff) <<  8;
            case 1: codepoint |= (str[i+0] & 0xff) <<  0;
        }
        codepoints.push_back(codepoint);
        i += cp_len;
    }
    auto obj = alloc_object<Simple_Array>(g.s_CHARACTER, static_cast<Fixnum>(codepoints.size()));
    auto array = obj.as_object()->simple_array();
    for (size_t i = 0; i < codepoints.size(); ++i)
    {
        array->at(i) = Value::wrap_character(codepoints[i]);
    }
    return obj;
}

Value GC::alloc_string(const std::string &str)
{
    return alloc_string(str.data(), str.size());
}




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
    out << "|R-stack " << std::setw(9) << r_stack_delta << " |         P-stack\n";
    out << "|==================|================\n";
    out << std::setfill('0');
    for (;max_size != 0; max_size--)
    {
        rt--;
        pt--;
        if (reinterpret_cast<uintptr_t>(rt) < reinterpret_cast<uintptr_t>(rb))
        {
            out << "| **************** |";
        }
        else
        {
            out << "| " << std::hex << std::setw(16) << reinterpret_cast<uintptr_t>(rt->ip) << " |";
        }

        if (reinterpret_cast<uintptr_t>(pt) < reinterpret_cast<uintptr_t>(pb))
        {
            out << "                ";
        }
        else
        {
            out << std::hex << std::setw(16) << reinterpret_cast<uintptr_t>(pt);
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

void VM_State::debug_dump(std::ostream &out, const std::string &tag, const uint8_t *ip, bool full) const
{
    //plat::clear_console();
    const Function *function;
    int lines_printed = 0;
    if (bytecode::Debug_Info::find_function(ip, &function))
    {
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

    for (; lines_printed <= 32; lines_printed++)
    {
        out << "\n";
    }

    if (full)
    {
        stack_dump(out, ~0u);
    }
    else
    {
        stack_dump(out);
    }
}

template<bool debuggable>
const uint8_t *VM_State::execute_impl(const uint8_t *ip)
{

#define BYTECODE_DEF(name, noperands, nargs, size, docstring) &&opcode_ ## name,
    void *computed_gotos[256] =
    {
        #include "bytecode.def"
    };

#define DISPATCH(name) case bytecode::Opcode::op_ ## name: opcode_ ## name:

#define DISPATCH_NEXT if constexpr (debuggable) break; else goto *computed_gotos[*ip];
#define EXEC switch(static_cast<bytecode::Opcode>(*ip))
#define DISPATCH_LOOP for (;;)

#if PROFILE_OPCODE_PAIRS
#define PREDICTED(name) //empty
#define PREDICT(name) //empty
#else
#define PREDICTED(name) predicted_ ## name:
#define PREDICT(name)                                                   \
    do {                                                                \
        if (static_cast<bytecode::Opcode>(*ip) == bytecode::Opcode::op_ ## name) \
        {                                                               \
            goto predicted_ ## name;                                       \
        }                                                               \
    } while (0)
#endif


#define TYPE_CHECK(what, typecheck, expected)                           \
    do {                                                                \
        if (!(what).typecheck) {                                        \
            signal_args = gc.list(g.s_TYPE_ERROR, (expected), (what));  \
            goto raise_signal;                                          \
        }                                                               \
    } while (0)

#define CHECK_FIXNUM(what) TYPE_CHECK(what, is_fixnum(), g.s_FIXNUM)
#define CHECK_CONS(what) TYPE_CHECK(what, is_cons(), g.s_CONS)
#define CHECK_LIST(what) TYPE_CHECK(what, is_list(), g.s_LIST)
#define CHECK_CHARACTER(what) TYPE_CHECK(what, is_character(), g.s_CHARACTER)
#define CHECK_SYMBOL(what) TYPE_CHECK(what, is_type(Object_Type::Symbol), g.s_SYMBOL)
#define CHECK_FILE_STREAM(what) TYPE_CHECK(what, is_type(Object_Type::File_Stream), g.s_FILE_STREAM)
#define CHECK_SIMPLE_ARRAY(what) TYPE_CHECK(what, is_type(Object_Type::Simple_Array), g.s_SIMPLE_ARRAY)
#define CHECK_SYSTEM_POINTER(what) TYPE_CHECK(what, is_type(Object_Type::System_Pointer), g.s_SYSTEM_POINTER)
#define CHECK_STRUCT(what) TYPE_CHECK(what, is_type(Object_Type::Structure), g.s_STRUCTURE)


    static_assert(sizeof(*ip) == 1, "pointer arithmetic will not work as expected.");
    Value signal_args;
    Value func;
    uint32_t nargs;
    enum class Call_Type
    {
        Doesnt_Push_Frame,
        Pushes_Frame,
    } call_type;
    const uint8_t *signal_raised_at_ip = nullptr;
    DISPATCH_LOOP
    {
        if constexpr (debuggable)
        {
            const auto opcode = static_cast<bytecode::Opcode>(*ip);
            if (g.debugger.breaking)
            {
                // @TODO: Fix debugger step over:
                // currently it just goes to the next instruction address which is ok in the case of
                // op_apply or op_funcall but it is incorrect in the case off op_jump and op_pop_jump_if_nil
                if ((g.debugger.command == Runtime_Globals::Debugger::Command::Step_Over &&
                     (g.debugger.addr0 == ip || g.debugger.addr1 == ip))
                    || g.debugger.command == Runtime_Globals::Debugger::Command::Step_Into)
                {
                    debug_dump(std::cout, "VM EXEC", ip);
                    auto &in = std::cin;
                    bool eat_newline = true;
                    switch (in.peek())
                    {
                        case 'c':
                            g.debugger.command = Runtime_Globals::Debugger::Command::Continue;
                            in.get();
                            break;
                        case 's':
                            g.debugger.command = Runtime_Globals::Debugger::Command::Step_Into;
                            in.get();
                            break;
                        case 'n':
                            g.debugger.command = Runtime_Globals::Debugger::Command::Step_Over;
                            in.get();
                            break;
                        case '\n':
                            in.get();
                            eat_newline = false;
                            break;
                    }

                    if (eat_newline && in.peek() == '\n')
                    {
                        in.get();
                    }

                    switch (g.debugger.command)
                    {
                        case Runtime_Globals::Debugger::Command::Continue:
                            g.debugger.breaking = false;
                            break;
                        case Runtime_Globals::Debugger::Command::Step_Into:
                            break;
                        case Runtime_Globals::Debugger::Command::Step_Over:
                            g.debugger.addr0 = ip + bytecode::opcode_size(opcode);
                            g.debugger.addr1 = nullptr;
                            if (opcode == bytecode::Opcode::op_pop_jump_if_nil)
                            {
                                auto offset = *reinterpret_cast<const int32_t*>(ip+1);
                                g.debugger.addr1 = ip + offset;
                            }
                            else if (opcode == bytecode::Opcode::op_jump)
                            {
                                auto offset = *reinterpret_cast<const int32_t*>(ip+1);
                                g.debugger.addr0 = ip + offset;
                            }
                            break;
                    }
                }
            }
#if PROFILE_OPCODE_PAIRS
            auto this_opcode = static_cast<bytecode::Opcode>(*ip);
            switch (this_opcode)
            {
                case bytecode::Opcode::op_halt:
                case bytecode::Opcode::op_return:
                case bytecode::Opcode::op_gotocall:
                case bytecode::Opcode::op_jump:
                case bytecode::Opcode::op_pop_jump_if_nil:
                    break;
                default: {
                    auto next_opcode = *(ip + bytecode::opcode_size(this_opcode));
                    m_opcode_pairs[Opcode_Pair{static_cast<short>(this_opcode), next_opcode}]++;
                } break;
            }
#endif
        } // if constexpr (debuggable)

        EXEC
        {
            DISPATCH(apply)
            {
                func = pop_param();
                nargs = *reinterpret_cast<const uint32_t*>(ip+1);
                if (nargs == 0)
                {
                    GC_GUARD();
                    signal_args = gc.list(g.s_SIMPLE_ERROR, gc.alloc_string("Too few arguments!"));
                    GC_UNGUARD();
                    goto raise_signal;
                }

                auto last_arg = pop_param();
                CHECK_LIST(last_arg);
                nargs--;
                while (!last_arg.is_nil())
                {
                    push_param(car(last_arg));
                    last_arg = cdr(last_arg);
                    nargs++;
                }
                call_type = Call_Type::Pushes_Frame;
                goto do_funcall;
            }

            DISPATCH(funcall)
            {
                PREDICTED(funcall);
                func = pop_param();
                nargs = *reinterpret_cast<const uint32_t*>(ip+1);
                call_type = Call_Type::Pushes_Frame;

                do_funcall:
                auto ofunc = func;
                if (symbolp(func))
                {
                    func = func.as_object()->symbol()->function();
                }

                if (func.is_lisp_primitive())
                {
                    bool raised_signal = false;
                    auto primitive = func.as_lisp_primitive();
                    auto result = primitive(m_stack_top - nargs, nargs, raised_signal);
                    m_stack_top -= nargs;
                    if (raised_signal)
                    {
                        signal_args = result;
                        goto raise_signal;
                    }
                    else
                    {
                        push_param(result);
                        ip += 1 + sizeof(nargs);
                    }
                    DISPATCH_NEXT;
                }

                if (func.is_type(Object_Type::Closure))
                {
                    // Although op_raise_signal also jumps here in a "funcall"-like way, it has
                    // already setup the stack in the state the closure expects to execute under
                    // so there is no need to push a frame for it here.
                    if (call_type == Call_Type::Pushes_Frame)
                    {
                        push_frame(ip+5, nargs);
                    }

                    m_current_closure = func;
                    auto closure = func.as_object()->closure();
                    auto function = closure->function();

                    // locals always start at first argument
                    m_locals = m_stack_top - nargs;

                    if (function->is_too_many_args(nargs))
                    {
                        GC_GUARD();
                        signal_args = gc.list(g.s_SIMPLE_ERROR,
                                              gc.alloc_string("Too many arguments!"),
                                              func,
                                              Value::wrap_fixnum(function->arity()),
                                              Value::wrap_fixnum(nargs));
                        GC_UNGUARD();
                        goto raise_signal;
                    }

                    if (function->is_too_few_args(nargs))
                    {
                        GC_GUARD();
                        signal_args = gc.list(g.s_SIMPLE_ERROR,
                                              gc.alloc_string("Too few arguments!"),
                                              func,
                                              Value::wrap_fixnum(function->arity()),
                                              Value::wrap_fixnum(nargs));
                        GC_UNGUARD();
                        goto raise_signal;
                    }

                    if (function->has_rest() && nargs > function->rest_index())
                    {
                        auto rest = to_list(m_locals+function->rest_index(),
                                            nargs - function->rest_index());
                        m_locals[function->rest_index()] = rest;
                    }

                    m_stack_top = m_locals + function->num_locals();
                    #if DEBUG > 1
                    {
                        assert(function->arity() <= function->num_locals());
                        auto start = m_locals + function->arity();
                        auto end = m_locals + function->num_locals();
                        for (; start != end; ++start)
                        {
                            *start = Value::nil();
                        }
                    }
                    #endif

                    ip = function->entrypoint(nargs);
                    DISPATCH_NEXT;
                }

                // error
                GC_GUARD();
                signal_args = gc.list(g.s_SIMPLE_ERROR, gc.alloc_string("Not a callable object"), ofunc, func);
                GC_UNGUARD();
                goto raise_signal;
            }

            DISPATCH(gotocall)
            {

                close_values(m_locals);

                func = pop_param();
                nargs = *reinterpret_cast<const uint32_t*>(ip+1);
                auto begin = m_stack_top - nargs;
                auto end = m_stack_top;
                m_stack_top = m_locals;
                std::copy(begin, end, m_stack_top);
                m_stack_top += nargs;

                call_type = Call_Type::Doesnt_Push_Frame;
                goto do_funcall;
            }

            DISPATCH(pop_handler_case)
                pop_handler_case();
                // fallthrough
            DISPATCH(return)
            {
                if (m_call_frame_top == m_call_frame_bottom)
                {
                    goto done;
                }

                close_values(m_locals);

                auto val = param_top();
                auto frame = pop_frame();
                set_frame(frame);
                push_param(val);
                if (frame.ip == nullptr)
                {
                    goto done;
                }
                ip = frame.ip;
                DISPATCH_NEXT;
            }

            DISPATCH(jump)
            {
                auto offset = *reinterpret_cast<const int32_t*>(ip+1);
                ip += offset;
                DISPATCH_NEXT;
            }

            DISPATCH(pop_jump_if_nil)
            {
                if (pop_param().is_nil())
                {
                    auto offset = *reinterpret_cast<const int32_t*>(ip+1);
                    ip += offset;
                }
                else
                {
                    ip += 5;
                }
                DISPATCH_NEXT;
            }

            DISPATCH(get_global)
            {
                auto index = *reinterpret_cast<const uint32_t*>(ip+1);
                push_param(g.global_value_slots[index]);
                ip += 5;
                DISPATCH_NEXT;
            }

            DISPATCH(set_global)
            {
                auto index = *reinterpret_cast<const uint32_t*>(ip+1);
                g.global_value_slots[index] = param_top();
                ip += 5;
                PREDICT(pop);
                DISPATCH_NEXT;
            }

            DISPATCH(get_local)
            {
                PREDICTED(get_local);
                auto index = *reinterpret_cast<const uint32_t*>(ip+1);
                push_param(m_locals[index]);
                ip += 5;
                PREDICT(push_value);
                PREDICT(get_local);
                DISPATCH_NEXT;
            }

            DISPATCH(set_local)
            {
                PREDICTED(set_local)
                auto index = *reinterpret_cast<const uint32_t*>(ip+1);
                m_locals[index] = param_top();
                ip += 5;
                PREDICT(pop);
                DISPATCH_NEXT;
            }

            DISPATCH(get_capture)
            {
                auto index = *reinterpret_cast<const uint32_t*>(ip+1);
                push_param(m_current_closure.as_object()->closure()->get_capture(index));
                ip += 5;
                DISPATCH_NEXT;
            }

            DISPATCH(set_capture)
            {
                auto index = *reinterpret_cast<const uint32_t*>(ip+1);
                m_current_closure.as_object()->closure()->set_capture(index, param_top());
                ip += 5;
                PREDICT(pop);
                DISPATCH_NEXT;
            }

            DISPATCH(function_value)
            {
                auto obj = *reinterpret_cast<const Value*>(ip+1);
                if (symbolp(obj))
                {
                    push_param(obj.as_object()->symbol()->function());
                }
                else
                {
                    push_param(obj);
                }
                ip += 1 + sizeof(obj);
                DISPATCH_NEXT;
            }

            DISPATCH(pop)
            {
                PREDICTED(pop);
                pop_param();
                ip += 1;
                PREDICT(get_local);
                DISPATCH_NEXT;
            }

            DISPATCH(push_value)
            {
                PREDICTED(push_value);
                auto val = *reinterpret_cast<const Value*>(ip+1);
                push_param(val);
                ip += 1 + sizeof(val);
                PREDICT(funcall);
                DISPATCH_NEXT;
            }

            DISPATCH(push_nil)
            {
                push_param(Value::nil());
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(push_fixnum_0)
            {
                push_param(Value::wrap_fixnum(0));
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(push_fixnum_1)
            {
                push_param(Value::wrap_fixnum(1));
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(instantiate_closure)
            {
                auto function = *reinterpret_cast<const Function* const*>(ip+1);
                auto instance = gc.alloc_object<Closure>(function);
                push_param(instance); // push this first so GC won't free it from under us
                auto closure = instance.as_object()->closure();
                if (function->has_captures())
                {

                    auto const &cap_offsets = function->capture_offsets();
                    for (size_t i = 0; i < cap_offsets.size(); ++i)
                    {
                        auto const &offs = cap_offsets[i];
                        Closure_Reference *ref;
                        if (offs.is_local)
                        {
                            ref = capture_closure_reference(m_locals + offs.index);
                        }
                        else
                        {
                            ref = m_current_closure.as_object()->closure()->get_reference(offs.index);
                        }
                        //printf("[%s] %d %p %s\n",
                        //       offs.name.c_str(),
                        //       offs.index,
                        //       ref->location(),
                        //       (ref->location() ? repr(ref->value()).c_str() : "#<nullptr>"));
                        closure->capture_reference(i, ref);
                    }
                }
                ip += 1 + sizeof(function);
                DISPATCH_NEXT;
            }

            DISPATCH(close_values)
            {
                auto n = *reinterpret_cast<const uint32_t*>(ip+1);

                close_values(m_stack_top-n);

                ip += 1 + sizeof(n);
                DISPATCH_NEXT;
            }

            DISPATCH(cons)
            {
                // Instead of popping twice, we use param_top(n) like this to serve as
                // a GC guard because before the GC may trigger a run before the cons
                // is allocated which may cause these values to be collected.
                auto val = gc.cons(param_top(-1), param_top(0));
                // THEN we pop the values after the cons is allocated.
                pop_params(2);
                push_param(val);
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(car)
            {
                auto o = pop_param();
                if (o.is_nil())
                {
                    push_param(o);
                }
                else
                {
                    CHECK_CONS(o);
                    push_param(car(o));
                }
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(cdr)
            {
                auto o = pop_param();
                if (o.is_nil())
                {
                    push_param(o);
                }
                else
                {
                    CHECK_CONS(o);
                    push_param(cdr(o));
                }
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(halt)
            {
                goto done;
            }

            DISPATCH(push_handler_case)
            {
                {
                    auto how_many = *reinterpret_cast<const uint32_t*>(ip+1);
                    auto branch = *reinterpret_cast<const uint32_t*>(ip+1+sizeof(how_many));
                    std::vector<Signal_Handler> handlers;
                    for (uint32_t i = 0; i < how_many; ++i)
                    {
                        auto tag = pop_param();
                        auto handler = pop_param();
                        handlers.push_back({tag, handler});
                    }
                    push_frame(ip + branch, 0);
                    push_handler_case(std::move(handlers));
                    ip += 1 + sizeof(how_many) + sizeof(branch);
                }
                DISPATCH_NEXT;
            }

            DISPATCH(raise_signal)
            {
                {
                    GC_GUARD();
                    // @Design, should we move the tag to be the first thing pushed since this just gets
                    // turned into a FUNCALL? The only reason to have the tag here is for easy access.
                    auto tag = pop_param();
                    auto nargs = *reinterpret_cast<const uint32_t*>(ip+1);
                    signal_args = to_list(m_stack_top - nargs, nargs);
                    signal_args = gc.cons(tag, signal_args);
                    GC_UNGUARD();
                }
                raise_signal:
                signal_raised_at_ip = ip;
                //bytecode::disassemble_maybe_function(std::cout, "SIGNAL", ip);
                Handler_Case restore;
                Signal_Handler handler;
                if (find_handler(first(signal_args), true, restore, handler))
                {
                    m_stack_top = restore.stack;
                    m_call_frame_top = restore.frame;
                    // By default signal_args includes the handler tag and a specific handler knows its
                    // own tag because it is labeled as such. In the case of a handler with the T tag it
                    // is unknown so we leave it, otherwise it is removed.
                    if (handler.tag != g.s_T)
                    {
                        signal_args = cdr(signal_args);
                    }
                    func = handler.handler;
                    nargs = 0;
                    while (!signal_args.is_nil())
                    {
                        ++nargs;
                        push_param(car(signal_args));
                        signal_args = cdr(signal_args);
                    }
                    call_type = Call_Type::Doesnt_Push_Frame;
                    goto do_funcall;
                }
                else
                {
                    auto top = m_call_frame_top;
                    auto bottom = m_call_frame_bottom;
                    m_call_frame_top = m_call_frame_bottom;
                    m_stack_top = m_stack_bottom;
                    throw Signal_Exception(signal_args, signal_raised_at_ip, top, bottom);
                }
            }

            DISPATCH(eq)
            {
                auto b = pop_param();
                auto a = pop_param();
                if (a == b)
                {
                    push_param(g.s_T);
                }
                else if (a.is_type(Object_Type::System_Pointer) &&
                         b.is_type(Object_Type::System_Pointer) &&
                         (a.as_object()->system_pointer() == b.as_object()->system_pointer()))
                {
                    push_param(g.s_T);
                }
                else
                {
                    push_param(Value::nil());
                }
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(rplaca)
            {
                auto b = pop_param();
                auto a = param_top();
                CHECK_CONS(a);
                set_car(a, b);
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(rplacd)
            {
                auto b = pop_param();
                auto a = param_top();
                CHECK_CONS(a);
                set_cdr(a, b);
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(aref)
            {
                auto subscript = pop_param();
                CHECK_FIXNUM(subscript);
                auto array_val = pop_param();
                CHECK_SIMPLE_ARRAY(array_val);
                auto array = array_val.as_object()->simple_array();
                auto index = subscript.as_fixnum();
                if (index < 0 || index >= array->size())
                {
                    signal_args = gc.list(g.s_INDEX_OUT_OF_BOUNDS_ERROR, subscript, array_val);
                    goto raise_signal;
                }
                push_param(array->at(index));
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(aset)
            {
                auto value = pop_param();
                auto subscript = pop_param();
                CHECK_FIXNUM(subscript);
                auto array_val = pop_param();
                CHECK_SIMPLE_ARRAY(array_val);
                auto array = array_val.as_object()->simple_array();
                auto index = subscript.as_fixnum();
                if (index < 0 || index >= array->size())
                {
                    signal_args = gc.list(g.s_INDEX_OUT_OF_BOUNDS_ERROR, subscript, array_val);
                    goto raise_signal;
                }
                auto type = array->element_type();
                if (type != g.s_T)
                {
                    if (type == g.s_FIXNUM && !value.is_fixnum())
                    {
                        CHECK_FIXNUM(value);
                    }
                    else if (type == g.s_CHARACTER && !value.is_character())
                    {
                        CHECK_CHARACTER(value);
                    }
                }
                array->at(index) = value;
                push_param(value);
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(debug_trap)
            {
                g.debugger.breaking = !param_top().is_nil();
                if (g.debugger.breaking)
                {
                    g.debugger.command = Runtime_Globals::Debugger::Command::Step_Into;
                }
                else
                {
                    g.debugger.command = Runtime_Globals::Debugger::Command::Continue;
                }
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(add)
            {
                /*
                  This is a type deduction algorithm with minimal branching
                  // good types
                  int, int = 3
                  flo, int = 6
                  int, flo = 9
                  flo, flo = 12

                  // fail types
                  no, no = 0
                  int, no = 1
                  flo, no = 4
                  no, int = 2
                  no, flo = 8
                 */
                auto b = pop_param();
                auto a = pop_param();

                char a_t = 0;
                a_t += a.is_fixnum();
                a_t += a.is_type(Object_Type::Float) * 4;

                char b_t = 0;
                b_t += b.is_fixnum() * 2;
                b_t += b.is_type(Object_Type::Float) * 8;

                char c_t = a_t + b_t;

                if (c_t == 3)
                {
                    push_param(a + b);
                }
                else if (c_t == 6)
                {
                    Float f = b.as_fixnum();
                    f += a.as_object()->to_float();
                    push_param(gc.alloc_object<Float>(f));
                }
                else if (c_t == 9)
                {
                    Float f = a.as_fixnum();
                    f += b.as_object()->to_float();
                    push_param(gc.alloc_object<Float>(f));
                }
                else if (c_t == 12)
                {
                    Float f = a.as_object()->to_float();
                    f += b.as_object()->to_float();
                    push_param(gc.alloc_object<Float>(f));
                }
                else
                {
                    GC_GUARD();
                    signal_args = gc.list(g.s_TYPE_ERROR, gc.list(g.s_OR, g.s_FIXNUM, g.s_FLOAT), a, b);
                    GC_UNGUARD();
                    goto raise_signal;
                }
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(add_1)
            {
                if (param_top().is_fixnum())
                {
                    param_top() += Value::wrap_fixnum(1);
                }
                else if (param_top().is_type(Object_Type::Float))
                {
                    Float f = pop_param().as_object()->to_float();
                    push_param(gc.alloc_object<Float>(f + 1.0));
                }
                ip += 1;
                PREDICT(set_local);
                DISPATCH_NEXT;
            }

            DISPATCH(sub)
            {
                /*
                  This is a type deduction algorithm with minimal branching
                  // good types
                  int, int = 3
                  flo, int = 6
                  int, flo = 9
                  flo, flo = 12

                  // fail types
                  no, no = 0
                  int, no = 1
                  flo, no = 4
                  no, int = 2
                  no, flo = 8
                 */
                auto b = pop_param();
                auto a = pop_param();

                char a_t = 0;
                a_t += a.is_fixnum();
                a_t += a.is_type(Object_Type::Float) * 4;

                char b_t = 0;
                b_t += b.is_fixnum() * 2;
                b_t += b.is_type(Object_Type::Float) * 8;

                char c_t = a_t + b_t;

                if (c_t == 3)
                {
                    push_param(a - b);
                }
                else if (c_t == 6)
                {
                    Float f = b.as_fixnum();
                    f -= a.as_object()->to_float();
                    push_param(gc.alloc_object<Float>(f));
                }
                else if (c_t == 9)
                {
                    Float f = a.as_fixnum();
                    f -= b.as_object()->to_float();
                    push_param(gc.alloc_object<Float>(f));
                }
                else if (c_t == 12)
                {
                    Float f = a.as_object()->to_float();
                    f -= b.as_object()->to_float();
                    push_param(gc.alloc_object<Float>(f));
                }
                else
                {
                    GC_GUARD();
                    signal_args = gc.list(g.s_TYPE_ERROR, gc.list(g.s_OR, g.s_FIXNUM, g.s_FLOAT), a, b);
                    GC_UNGUARD();
                    goto raise_signal;
                }
                ip += 1;
                DISPATCH_NEXT;
            }

            DISPATCH(sub_1)
            {
                if (param_top().is_fixnum())
                {
                    param_top() -= Value::wrap_fixnum(1);
                }
                else if (param_top().is_type(Object_Type::Float))
                {
                    Float f = pop_param().as_object()->to_float();
                    push_param(gc.alloc_object<Float>(f - 1.0));
                }
                ip += 1;
                DISPATCH_NEXT;
            }
        }
    }
    done:

    return ip;
}

#undef TYPE_CHECK
#define TYPE_CHECK(what, typecheck, expected)                   \
    do {                                                        \
        if (!(what).typecheck) {                                \
            raised_signal = true;                               \
            return gc.list(g.s_TYPE_ERROR, (expected), (what)); \
        }                                                       \
    } while (0)

#define CHECK_STRING(what)                                      \
    do {                                                        \
        if (!stringp(what)) {                                   \
            raised_signal = true;                               \
            return gc.list(g.s_TYPE_ERROR, g.s_STRING, (what)); \
        }                                                       \
    } while (0)

#define CHECK_NUMBER(what)                                      \
    do {                                                        \
        if (!numberp(what)) {                                   \
            raised_signal = true;                               \
            return gc.list(g.s_TYPE_ERROR, gc.list(g.s_OR, g.s_FIXNUM, g.s_FLOAT), (what)); \
        }                                                       \
    } while (0)

#define CHECK_NARGS_AT_LEAST(n)                                       \
    do {                                                                \
        if (nargs < (n)) {                                             \
            raised_signal = true;                                       \
            return gc.list(g.s_SIMPLE_ERROR, gc.alloc_string("Argument count mismatch"), Value::wrap_fixnum(n), Value::wrap_fixnum(nargs)); \
        }                                                               \
    } while (0)

#define CHECK_NARGS_EXACTLY(n)                                        \
    do {                                                                \
        if (nargs != (n)) {                                            \
            raised_signal = true;                                       \
            return gc.list(g.s_SIMPLE_ERROR, gc.alloc_string("Argument count mismatch"), Value::wrap_fixnum(n), Value::wrap_fixnum(nargs)); \
        }                                                               \
    } while (0)


static void set_global(compiler::Scope *scope, Value symbol_value, Value value);

struct Function_Initializer
{
    using Cpp_Function = Value (*)(Value *args, uint32_t nargs, bool &raised_signal);

    std::string lisp_name;
    std::string cpp_name;
    Package *package;
    Cpp_Function cpp_function;
    bool is_exported;
};
static std::vector<Function_Initializer> g_function_initializers;

struct Function_Initializer_With_Defun
{
    Function_Initializer_With_Defun(const std::string &lisp_name,
                                    const std::string &cpp_name,
                                    const std::string &package_name,
                                    Function_Initializer::Cpp_Function cpp_function,
                                    bool is_exported)
    {
        auto package = g.packages.find_or_create(package_name);
        g_function_initializers.push_back({lisp_name, cpp_name, package, cpp_function, is_exported});
    }

    Function_Initializer_With_Defun(const std::string &lisp_name,
                                    const std::string &cpp_name,
                                    Package *package,
                                    Function_Initializer::Cpp_Function cpp_function,
                                    bool is_exported)
    {
        g_function_initializers.push_back({lisp_name, cpp_name, package, cpp_function, is_exported});
    }
};

namespace primitives
{

#define CONCAT_IMPL(x, y) x ## y
#define MACRO_CONCAT(x, y) CONCAT_IMPL(x, y)
#define GENSYM(pfx) MACRO_CONCAT(pfx, __COUNTER__)

#define DEFUN(lisp_name, cpp_name, package, export)                 \
    Value cpp_name(Value *args, uint32_t nargs, bool &raised_signal); \
    static Function_Initializer_With_Defun GENSYM(pfx) (lisp_name, #cpp_name, package, cpp_name, export); \
    Value cpp_name(Value *args, uint32_t nargs, bool &raised_signal)

///////////////////////////////////////////////////////////////////////
// Internal Functions

DEFUN("%PRINT", func_print, g.core(), false)
{
    for (uint32_t i = 0; i < nargs; ++i)
    {
        printf("%s ", repr(args[i]).c_str());
    }
    printf("\n");
    return Value::nil();
}

DEFUN("%DISASSEMBLE", func_disassemble, g.kernel(), false)
{
    CHECK_NARGS_EXACTLY(1);
    auto expr = args[0];
    std::string tag = "DISASSEMBLY";
    if (expr.is_cons())
    {
        try
        {
            auto expanded = macro_expand_impl(expr, *THE_LISP_VM);
            bytecode::Emitter e(compiler::THE_ROOT_SCOPE);
            compiler::compile(e, expanded, true);
            e.lock();
            bytecode::disassemble(std::cout, tag, e);
        }
        catch (VM_State::Signal_Exception ex)
        {
            raised_signal = true;
            return ex.what;
        }
    }
    else if (expr.is_fixnum())
    {
        auto ptr = expr.as_fixnum();
        auto val = Value(static_cast<Value::Bits_Type>(ptr));
        if (val.is_type(Object_Type::Closure))
        {
            auto closure = expr.as_object()->closure();
            auto symbol = bytecode::find_symbol_with_function(closure->function());
            if (symbol != nullptr)
                tag = symbol->qualified_name();
            bytecode::disassemble(std::cout, tag, closure->function(), closure->function()->main_entry());
        }
        else
        {
            bytecode::disassemble(std::cout, tag, reinterpret_cast<uint8_t*>(ptr));
        }
    }
    else if (expr.is_type(Object_Type::Closure))
    {
        auto closure = expr.as_object()->closure();
        auto symbol = bytecode::find_symbol_with_function(closure->function());
        if (symbol != nullptr)
            tag = symbol->qualified_name();
        bytecode::disassemble(std::cout, tag, closure->function(), closure->function()->main_entry());
    }
    return Value::nil();
}

DEFUN("%DEFINE-FUNCTION", func_define_function, g.kernel(), false)
{
    CHECK_NARGS_EXACTLY(2);
    auto sym = args[0];
    CHECK_SYMBOL(sym);
    auto func = args[1];
    if (!func.is_lisp_primitive() && !func.is_type(Object_Type::Closure))
    {
        raised_signal = true;
        return gc.list(g.s_TYPE_ERROR, g.s_FUNCTION, func);
    }
    sym.as_object()->symbol()->function(func);
    return sym;
}

static
bool check_string_like(Value &arg, Value &out_signal_args, std::string &out_string)
{
    // Helper to check if arg is a string-like, storing the string representation in out_string.
    // Returns true if a signal should be raised and stores the signal arguments in out_signal_args.
    if (symbolp(arg))
    {
        out_string = arg.as_object()->symbol()->name();
    }
    else if (stringp(arg))
    {
        out_string = lisp_string_to_native_string(arg);
    }
    else
    {
        GC_GUARD();
        out_signal_args = gc.list(g.s_TYPE_ERROR,
                           gc.list(g.get_symbol("OR"),
                                   g.get_symbol("STRING"),
                                   g.get_symbol("SYMBOL"),
                                   g.get_symbol("KEYWORD")),
                           arg);
        GC_UNGUARD();
        return true;
    }
    return false;
}

static
bool check_package(Value &arg, Package **out_pkg, Value &out_signal_args)
{
    // Helper to check type of arg as a package or package designator: i.e.
    // symbol, string, keyword, or package.
    // returns true to raise signal and false if arg is a package designator.
    // stores the found package in out_pkg.
    if (symbolp(arg))
    {
        *out_pkg = g.packages.find(arg.as_object()->symbol()->name());
    }
    else if (stringp(arg))
    {
        *out_pkg = g.packages.find(lisp_string_to_native_string(arg));
    }
    else if (arg.is_type(Object_Type::Package))
    {
        *out_pkg = arg.as_object()->package();
    }
    else
    {
        GC_GUARD();
        out_signal_args = gc.list(g.s_TYPE_ERROR,
                           gc.list(g.get_symbol("OR"),
                                   g.get_symbol("STRING"),
                                   g.get_symbol("SYMBOL"),
                                   g.get_symbol("KEYWORD"),
                                   g.get_symbol("PACKAGE")),
                           arg);
        GC_UNGUARD();
        return true;
    }
    return false;
}

DEFUN("%PACKAGE-NAME", func_package_name, g.kernel(), false)
{
    CHECK_NARGS_EXACTLY(1);
    Package *package = nullptr;
    {
        Value res;
        raised_signal = check_package(args[0], &package, res);
        if (raised_signal)
        {
            return res;
        }
    }

    if (package == nullptr)
    {
        raised_signal = true;
        GC_GUARD();
        auto res = gc.list(g.s_SIMPLE_ERROR, gc.alloc_string("Package does not exist"), args[0]);
        GC_UNGUARD();
        return res;
    }

    return gc.alloc_string(args[0].as_object()->package()->name());
}

DEFUN("%MAKE-PACKAGE", func_make_package, g.kernel(), false)
{
    CHECK_NARGS_EXACTLY(1);
    std::string package_name;
    {
        Value signal_args;
        raised_signal = check_string_like(args[0], signal_args, package_name);
        if (raised_signal)
        {
            return signal_args;
        }
    }

    if (g.packages.find(package_name))
    {
        raised_signal = true;
        GC_GUARD();
        auto res = gc.list(g.s_SIMPLE_ERROR,
                           gc.alloc_string("A package with the same name already exists."),
                           args[0]);
        GC_UNGUARD();
        return res;
    }

    auto package = g.packages.find_or_create(package_name);
    return package->as_lisp_value();
}

DEFUN("%FIND-PACKAGE", func_find_package, g.kernel(), false)
{
    CHECK_NARGS_EXACTLY(1);
    std::string package_name;
    {
        Value signal_args;
        raised_signal = check_string_like(args[0], signal_args, package_name);
        if (raised_signal)
        {
            return signal_args;
        }
    }

    auto pkg = g.packages.find(package_name);
    return pkg ? pkg->as_lisp_value() : Value::nil();
}

DEFUN("%USE-PACKAGE", func_use_package, g.kernel(), false)
{
    CHECK_NARGS_AT_LEAST(1);

    std::string package_name;
    {
        Value signal_args;
        raised_signal = check_string_like(args[0], signal_args, package_name);
        if (raised_signal)
        {
            return signal_args;
        }
    }
    Package *package_to_use = g.packages.find(package_name);

    if (package_to_use == nullptr)
    {
        raised_signal = true;
        GC_GUARD();
        auto res = gc.list(g.s_SIMPLE_ERROR,
                           gc.alloc_string("Package does not exist"),
                           args[0]);
        GC_UNGUARD();
        return res;
    }

    auto in_package = g.packages.current();
    if (nargs > 1)
    {
        Value signal_args;
        raised_signal = check_package(args[1], &in_package, signal_args);
        if (raised_signal)
        {
            return signal_args;
        }
    }

    if (in_package == nullptr)
    {
        raised_signal = true;
        GC_GUARD();
        auto res = gc.list(g.s_SIMPLE_ERROR, gc.alloc_string("Package does not exist"), args[1]);
        GC_UNGUARD();
        return res;
    }

    in_package->inherit(package_to_use);
    return g.s_T;
}

DEFUN("%IN-PACKAGE", func_in_package, g.kernel(), false)
{
    CHECK_NARGS_EXACTLY(1);

    Package *package_to_use;
    if (args[0].is_type(Object_Type::Package))
    {
        package_to_use = args[0].as_object()->package();
    }
    else
    {
        std::string package_name;
        {
            Value signal_args;
            raised_signal = check_string_like(args[0], signal_args, package_name);
            if (raised_signal)
            {
                return signal_args;
            }
        }
        package_to_use = g.packages.find(package_name);
    }

    if (package_to_use == nullptr)
    {
        raised_signal = true;
        GC_GUARD();
        auto res = gc.list(g.s_SIMPLE_ERROR,
                           gc.alloc_string("Package does not exist"),
                           args[0]);
        GC_UNGUARD();
        return res;
    }

    g.packages.in_package(package_to_use);

    set_global(compiler::THE_ROOT_SCOPE,
               g.get_symbol("*PACKAGE*"),
               package_to_use->as_lisp_value());
    return package_to_use->as_lisp_value();
}

DEFUN("%+", func_plus, g.kernel(), false)
{
    /***
        (+ &rest fixnums)
    */

    if (only_fixnums(args, nargs))
    {
        auto result = Value::wrap_fixnum(0);
        for (uint32_t i = 0; i < nargs; ++i)
        {
            auto tmp = args[i];
            result += tmp;
        }
        return result;
    }
    else
    {
        Float result = 0.0;
        for (uint32_t i = 0; i < nargs; ++i)
        {
            auto tmp = args[i];
            if (tmp.is_fixnum())
            {
                result += tmp.as_fixnum();
            }
            else if (tmp.is_type(Object_Type::Float))
            {
                result += tmp.as_object()->to_float();
            }
            else
            {
                raised_signal = true;
                return gc.list(g.s_TYPE_ERROR, gc.list(g.s_OR, g.s_FIXNUM, g.s_FLOAT), tmp);
            }
        }
        return gc.alloc_object<Float>(result);
    }
}

DEFUN("%-", func_minus, g.kernel(), false)
{
    /***
        (- &rest fixnums)
    */
    if (only_fixnums(args, nargs))
    {
        auto result = Value::wrap_fixnum(0);
        if (nargs == 0)
        {
            ;
        }
        else if (nargs == 1)
        {
            result -= args[0];
        }
        else
        {
            result = args[0];
            for (uint32_t i = 1; i < nargs; ++i)
            {
                result -= args[i];
            }
        }
        return result;
    }
    else
    {
        Float result = 0.0;
        if (nargs == 1)
        {
            // No need to check for fixnum because one arg that's a fixnum executes the
            // previous branch.
            if (args[0].is_type(Object_Type::Float))
            {
                result -= args[0].as_object()->to_float();
            }
            else
            {
                raised_signal = true;
                return gc.list(g.s_TYPE_ERROR, gc.list(g.s_OR, g.s_FIXNUM, g.s_FLOAT), args[0]);
            }
        }
        else
        {
            if (args[0].is_fixnum())
            {
                result = args[0].as_fixnum();
            }
            else if (args[0].is_type(Object_Type::Float))
            {
                result = args[0].as_object()->to_float();
            }
            else
            {
                raised_signal = true;
                return gc.list(g.s_TYPE_ERROR, gc.list(g.s_OR, g.s_FIXNUM, g.s_FLOAT), args[0]);
            }
            for (uint32_t i = 1; i < nargs; ++i)
            {
                if (args[i].is_fixnum())
                {
                    result -= args[i].as_fixnum();
                }
                else if (args[i].is_type(Object_Type::Float))
                {
                    result -= args[i].as_object()->to_float();
                }
                else
                {
                    raised_signal = true;
                    return gc.list(g.s_TYPE_ERROR, gc.list(g.s_OR, g.s_FIXNUM, g.s_FLOAT), args[0]);
                }
            }
        }
        return gc.alloc_object<Float>(result);
    }
}

DEFUN("%*", func_multiply, g.kernel(), false)
{
    /***
        (* &rest fixnums)
    */
    if (only_fixnums(args, nargs))
    {
        Fixnum result = 1;
        for (uint32_t i = 0; i < nargs; ++i)
        {
            auto tmp = args[i];
            result *= tmp.as_fixnum();
        }
        return Value::wrap_fixnum(result);
    }
    else
    {
        Float result = 1.0;
        for (uint32_t i = 0; i < nargs; ++i)
        {
            auto tmp = args[i];
            if (tmp.is_fixnum())
            {
                result *= tmp.as_fixnum();
            }
            else if (tmp.is_type(Object_Type::Float))
            {
                result *= tmp.as_object()->to_float();
            }
            else
            {
                raised_signal = true;
                return gc.list(g.s_TYPE_ERROR, gc.list(g.s_OR, g.s_FIXNUM, g.s_FLOAT), tmp);
            }
        }
        return gc.alloc_object<Float>(result);
    }
}

DEFUN("%/", func_divide, g.kernel(), false)
{
    /***
        (/ x y)
    */
    CHECK_NARGS_EXACTLY(2);
    if (only_fixnums(args, nargs))
    {
        auto x = args[0].as_fixnum();
        auto y = args[1].as_fixnum();
        if (y == 0)
        {
            raised_signal = true;
            return gc.list(g.s_DIVIDE_BY_ZERO_ERROR, args[0], args[1]);
        }
        return Value::wrap_fixnum(x / y);
    }
    else
    {
        Float x, y;
        if (args[0].is_fixnum())
        {
            x = args[0].as_fixnum();
        }
        else if (args[0].is_type(Object_Type::Float))
        {
            x = args[0].as_object()->to_float();
        }
        else
        {
            raised_signal = true;
            return gc.list(g.s_TYPE_ERROR, gc.list(g.s_OR, g.s_FIXNUM, g.s_FLOAT), args[0]);
        }
        if (args[1].is_fixnum())
        {
            y = args[1].as_fixnum();
        }
        else if (args[1].is_type(Object_Type::Float))
        {
            y = args[1].as_object()->to_float();
        }
        else
        {
            raised_signal = true;
            return gc.list(g.s_TYPE_ERROR, gc.list(g.s_OR, g.s_FIXNUM, g.s_FLOAT), args[1]);
        }

        return gc.alloc_object<Float>(x / y);
    }
}

DEFUN("%FLOAT-STRING", func_float_string, g.kernel(), false)
{
    CHECK_NARGS_EXACTLY(1);
    Float f;
    if (args[0].is_fixnum())
    {
        f = args[0].as_fixnum();
    }
    else if (args[0].is_type(Object_Type::Float))
    {
        f = args[0].as_object()->to_float();
    }
    else
    {
        raised_signal = true;
        return gc.list(g.s_TYPE_ERROR, gc.list(g.s_OR, g.s_FIXNUM, g.s_FLOAT), args[0]);
    }

    std::stringstream ss;
    ss << std::fixed << std::setprecision(15) << f;
    std::string result_str = ss.str();
    result_str.erase(std::find_if(result_str.rbegin(),
                         result_str.rend(),
                         [](int ch) {
                             return ch != '0';
                         }).base()+1,
            result_str.end());
    return gc.alloc_string(result_str);
}

DEFUN("%FLOAT-COMPONENTS", func_float_components, g.kernel(), false)
{
    CHECK_NARGS_EXACTLY(1);
    union {
        double f;
        uint64_t i;
    } u;
    if (args[0].is_fixnum())
    {
        u.f = args[0].as_fixnum();
    }
    else if (args[0].is_type(Object_Type::Float))
    {
        u.f = args[0].as_object()->to_float();
    }
    else
    {
        raised_signal = true;
        return gc.list(g.s_TYPE_ERROR, gc.list(g.s_OR, g.s_FIXNUM, g.s_FLOAT), args[0]);
    }

    auto negp = (u.i >> 63) & 1;
    auto expt = (u.i >> 51) & 0x7ff;
    auto mant =  u.i & 0x000fffffffffffff;
    return gc.list((negp ? g.s_T : Value::nil()), Value::wrap_fixnum(expt), Value::wrap_fixnum(mant));
}

DEFUN("%FLOOR", func_floor, g.kernel(), false)
{
    CHECK_NARGS_AT_LEAST(1);
    if (nargs == 1)
    {
        if (args[0].is_fixnum())
        {
            return args[0];
        }
        else if (args[0].is_type(Object_Type::Float))
        {
            return Value::wrap_fixnum(args[0].as_object()->to_float());
        }
        else
        {
            raised_signal = true;
            return gc.list(g.s_TYPE_ERROR, gc.list(g.s_OR, g.s_FIXNUM, g.s_FLOAT), args[0]);
        }
    }
    else
    {
        CHECK_NARGS_EXACTLY(2);
        Float x, y;
        if (args[0].is_fixnum())
        {
            x = args[0].as_fixnum();
        }
        else if (args[0].is_type(Object_Type::Float))
        {
            x = args[0].as_object()->to_float();
        }
        else
        {
            raised_signal = true;
            return gc.list(g.s_TYPE_ERROR, gc.list(g.s_OR, g.s_FIXNUM, g.s_FLOAT), args[0]);
        }
        if (args[1].is_fixnum())
        {
            y = args[1].as_fixnum();
        }
        else if (args[1].is_type(Object_Type::Float))
        {
            y = args[1].as_object()->to_float();
        }
        else
        {
            raised_signal = true;
            return gc.list(g.s_TYPE_ERROR, gc.list(g.s_OR, g.s_FIXNUM, g.s_FLOAT), args[1]);
        }

        return Value::wrap_fixnum(x / y);
    }
}

DEFUN("%FLOAT-DIVIDE", func_float_divide, g.kernel(), false)
{
    /***
        (float-divide x y)
    */
    CHECK_NARGS_EXACTLY(2);
    Float x, y;
    if (args[0].is_fixnum())
    {
        x = args[0].as_fixnum();
    }
    else if (args[0].is_type(Object_Type::Float))
    {
        x = args[0].as_object()->to_float();
    }
    else
    {
        raised_signal = true;
        return gc.list(g.s_TYPE_ERROR, gc.list(g.s_OR, g.s_FIXNUM, g.s_FLOAT), args[0]);
    }
    if (args[1].is_fixnum())
    {
        y = args[1].as_fixnum();
    }
    else if (args[1].is_type(Object_Type::Float))
    {
        y = args[1].as_object()->to_float();
    }
    else
    {
        raised_signal = true;
        return gc.list(g.s_TYPE_ERROR, gc.list(g.s_OR, g.s_FIXNUM, g.s_FLOAT), args[1]);
    }
    return gc.alloc_object<Float>(x / y);
}

DEFUN("%=", func_num_equal, g.kernel(), false)
{
    /***
        (= n &rest more-fixnums)
    */
    CHECK_NARGS_AT_LEAST(1);
    if (only_fixnums(args, nargs))
    {
        auto n = args[0];
        for (uint32_t i = 1; i < nargs; ++i)
        {
            if (args[i] != n)
            {
                return Value::nil();
            }
        }
        return g.s_T;
    }
    else
    {
        Float n;
        if (args[0].is_fixnum())
        {
            n = args[0].as_fixnum();
        }
        else if (args[0].is_type(Object_Type::Float))
        {
            n = args[0].as_object()->to_float();
        }
        else
        {
            raised_signal = true;
            return gc.list(g.s_TYPE_ERROR, gc.list(g.s_OR, g.s_FIXNUM, g.s_FLOAT), args[0]);
        }

        for (uint32_t i = 1; i < nargs; ++i)
        {
            if (args[i].is_fixnum())
            {
                if (args[i].as_fixnum() != n)
                {
                    return Value::nil();
                }
            }
            else if (args[i].is_type(Object_Type::Float))
            {
                if (args[i].as_object()->to_float() != n)
                {
                    return Value::nil();
                }
            }
            else
            {
                raised_signal = true;
                return gc.list(g.s_TYPE_ERROR, gc.list(g.s_OR, g.s_FIXNUM, g.s_FLOAT), args[i]);
            }
        }
        return g.s_T;
    }
}

DEFUN("%<", func_num_less, g.kernel(), false)
{
    /***
        (< a b &rest more-fixnums)
    */
    CHECK_NARGS_AT_LEAST(2);
    if (only_fixnums(args, nargs))
    {
        auto a = args[0];
        auto b = args[1];
        bool result = a.as_fixnum() < b.as_fixnum();
        if (result)
        {
            a = b;
            for (uint32_t i = 2; i < nargs; ++i)
            {
                b = args[i];
                result = a.as_fixnum() < b.as_fixnum();
                if (result == false)
                {
                    break;
                }
                a = b;
            }
        }
        return result ? g.s_T : Value::nil();
    }
    else
    {
        Float a, b;
        if (args[0].is_fixnum())
        {
            a = args[0].as_fixnum();
        }
        else if (args[0].is_type(Object_Type::Float))
        {
            a = args[0].as_object()->to_float();
        }
        else
        {
            raised_signal = true;
            return gc.list(g.s_TYPE_ERROR, gc.list(g.s_OR, g.s_FIXNUM, g.s_FLOAT), args[0]);
        }
        if (args[1].is_fixnum())
        {
            b = args[1].as_fixnum();
        }
        else if (args[1].is_type(Object_Type::Float))
        {
            b = args[1].as_object()->to_float();
        }
        else
        {
            raised_signal = true;
            return gc.list(g.s_TYPE_ERROR, gc.list(g.s_OR, g.s_FIXNUM, g.s_FLOAT), args[1]);
        }

        bool result = a < b;
        if (result)
        {
            a = b;
            for (uint32_t i = 2; i < nargs; ++i)
            {
                if (args[i].is_fixnum())
                {
                    b = args[i].as_fixnum();
                }
                else if (args[i].is_type(Object_Type::Float))
                {
                    b = args[i].as_object()->to_float();
                }
                else
                {
                    raised_signal = true;
                    return gc.list(g.s_TYPE_ERROR, gc.list(g.s_OR, g.s_FIXNUM, g.s_FLOAT), args[i]);
                }

                result = a < b;
                if (result == false)
                {
                    break;
                }
                a = b;
            }
        }
        return result ? g.s_T : Value::nil();
    }
}

DEFUN("%>", func_num_greater, g.kernel(), false)
{
    /***
        (> a b &rest more-fixnums)
    */
    CHECK_NARGS_AT_LEAST(2);
    if (only_fixnums(args, nargs))
    {
        auto a = args[0];
        auto b = args[1];
        bool result = a.as_fixnum() > b.as_fixnum();
        if (result)
        {
            a = b;
            for (uint32_t i = 2; i < nargs; ++i)
            {
                b = args[i];
                result = a.as_fixnum() > b.as_fixnum();
                if (result == false)
                {
                    break;
                }
                a = b;
            }
        }
        return result ? g.s_T : Value::nil();
    }
    else
    {
        Float a, b;
        if (args[0].is_fixnum())
        {
            a = args[0].as_fixnum();
        }
        else if (args[0].is_type(Object_Type::Float))
        {
            a = args[0].as_object()->to_float();
        }
        else
        {
            raised_signal = true;
            return gc.list(g.s_TYPE_ERROR, gc.list(g.s_OR, g.s_FIXNUM, g.s_FLOAT), args[0]);
        }
        if (args[1].is_fixnum())
        {
            b = args[1].as_fixnum();
        }
        else if (args[1].is_type(Object_Type::Float))
        {
            b = args[1].as_object()->to_float();
        }
        else
        {
            raised_signal = true;
            return gc.list(g.s_TYPE_ERROR, gc.list(g.s_OR, g.s_FIXNUM, g.s_FLOAT), args[1]);
        }

        bool result = a > b;
        if (result)
        {
            a = b;
            for (uint32_t i = 2; i < nargs; ++i)
            {
                if (args[i].is_fixnum())
                {
                    b = args[i].as_fixnum();
                }
                else if (args[i].is_type(Object_Type::Float))
                {
                    b = args[i].as_object()->to_float();
                }
                else
                {
                    raised_signal = true;
                    return gc.list(g.s_TYPE_ERROR, gc.list(g.s_OR, g.s_FIXNUM, g.s_FLOAT), args[i]);
                }

                result = a > b;
                if (result == false)
                {
                    break;
                }
                a = b;
            }
        }
        return result ? g.s_T : Value::nil();
    }
}

DEFUN("%FILE-TELLG", func_file_tellg, g.kernel(), false)
{
    /***
        (file-tellg stream)
    */
    CHECK_NARGS_EXACTLY(1);
    CHECK_FILE_STREAM(args[0]);
    auto &stm = args[0].as_object()->file_stream()->stream();
    auto pos = stm.tellg();
    return Value::wrap_fixnum(pos);
}

DEFUN("%FILE-SEEKG", func_file_seekg, g.kernel(), false)
{
    /***
        (file-seekg stream offset dir)
    */
    CHECK_NARGS_EXACTLY(3);
    CHECK_FILE_STREAM(args[0]);
    CHECK_FIXNUM(args[1]);
    CHECK_SYMBOL(args[2]);
    auto &stm = args[0].as_object()->file_stream()->stream();
    auto pos = std::ios_base::cur;
    if (args[2] == g.s_BEGINNING)
    {
        pos = std::ios_base::beg;
    }
    else if (args[2] == g.s_END)
    {
        pos = std::ios_base::end;
    }
    else if (args[2] == g.s_CURRENT)
    {
        pos = std::ios_base::cur;
    }
    else
    {
        raised_signal = true;
        GC_GUARD();
        auto signal_args = gc.list(g.s_SIMPLE_ERROR,
                                   gc.alloc_string("Expected one of"),
                                   gc.list(g.s_BEGINNING, g.s_CURRENT, g.s_END),
                                   args[2]);
        GC_UNGUARD();
        return signal_args;
    }
    stm.seekg(args[1].as_fixnum(), pos);
    return Value::nil();
}

DEFUN("%FILE-WRITE", func_file_write, g.kernel(), false)
{
    /***
        (file-write stream object)
    */
    CHECK_NARGS_EXACTLY(2);
    CHECK_FILE_STREAM(args[0]);
    auto &stm = args[0].as_object()->file_stream()->stream();
    auto obj = args[1];
    auto pos = stm.tellg();
    stm << repr(obj);
    auto bytes_written = stm.tellg() - pos;
    stm.flush();
    return Value::wrap_fixnum(bytes_written);
}

DEFUN("%FILE-PUTCHAR", func_file_putchar, g.kernel(), false)
{
    /***
        (file-putchar stream character)
    */

    CHECK_NARGS_EXACTLY(2);
    CHECK_FILE_STREAM(args[0]);
    auto stm = args[0].as_object()->file_stream();
    CHECK_CHARACTER(args[1]);
    auto codepoint = args[1].as_character();
    auto bytes_written = stm->write_character(codepoint);
    if (codepoint == '\n')
    {
        stm->flush();
    }
    return Value::wrap_fixnum(bytes_written);
}

DEFUN("%FILE-PUTS", func_file_puts, g.kernel(), false)
{
    /***
        (file-puts stream string)
    */

    CHECK_NARGS_EXACTLY(2);
    CHECK_FILE_STREAM(args[0]);
    CHECK_STRING(args[1]);
    auto &stm = args[0].as_object()->file_stream()->stream();
    auto pos = stm.tellg();
    stm << lisp_string_to_native_string(args[1]);
    auto bytes_written = stm.tellg() - pos;
    stm.flush();
    return Value::wrap_fixnum(bytes_written);
}

DEFUN("%TYPE-OF", func_type_of, g.kernel(), false)
{
    /***
        (type-of object)
    */
    CHECK_NARGS_EXACTLY(1);
    auto it = args[0];
    if (it.is_fixnum())
    {
        return g.s_FIXNUM;
    }
    if (it.is_nil())
    {
        return g.s_NULL;
    }
    if (it.is_cons())
    {
        return g.s_CONS;
    }
    if (it.is_character())
    {
        return g.s_CHARACTER;
    }
    if (it.is_object())
    {
        switch (it.as_object()->type())
        {
            case Object_Type::Symbol: return g.s_SYMBOL;
            case Object_Type::Closure: return g.s_FUNCTION;
            case Object_Type::Simple_Array:
            {
                auto array = it.as_object()->simple_array();
                return gc.list(g.s_SIMPLE_ARRAY,
                               array->element_type(),
                               Value::wrap_fixnum(array->size()));
            };
            case Object_Type::File_Stream: return g.s_FILE_STREAM;
            case Object_Type::System_Pointer: return g.s_SYSTEM_POINTER;
            case Object_Type::Structure: return it.as_object()->structure()->type_name();
            case Object_Type::Package: return g.s_PACKAGE;
            case Object_Type::Float: return g.s_FLOAT;
        }
        return Value::nil();
    }
    if (it.is_lisp_primitive())
    {
        return g.s_FUNCTION;
    }
    if (it == g.s_T)
    {
        return g.s_BOOLEAN;
    }
    raised_signal = true;
    GC_GUARD();
    auto res = gc.list(g.s_SIMPLE_ERROR, gc.alloc_string("Cannot determine type of object."), it);
    GC_UNGUARD();
    return res;
}

DEFUN("%READ", func_read, g.kernel(), false)
{
    /***
        (read &optional file-stream eof-error-p eof-value)
    */

    bool eof_error_p = true;
    if (nargs > 1)
    {
        eof_error_p = !args[1].is_nil();
    }
    auto eof_value = Value::nil();
    if (nargs > 2)
    {
        eof_value = args[2];
    }

    if (nargs == 0)
    {
        Value result;
        if (!read_gc_paused(std::cin, result))
        {
            // @FIXME: This should be checking for errors, not assuming EOF
            if (eof_error_p)
            {
                raised_signal = true;
                return gc.list(g.s_END_OF_FILE);
            }
            return eof_value;
        }
        return result;
    }
    else
    {
        CHECK_FILE_STREAM(args[0]);
        Value result;
        if (!read_gc_paused(args[0].as_object()->file_stream()->stream(), result))
        {
            // @FIXME: This should be checking for errors, not assuming EOF
            if (eof_error_p)
            {
                raised_signal = true;
                return gc.list(g.s_END_OF_FILE);
            }
            return eof_value;
        }
        return result;
    }
}

DEFUN("%MACRO-EXPAND", func_macro_expand, g.kernel(), false)
{
    /***
        (macro-expand expr)
    */
    CHECK_NARGS_EXACTLY(1);
    return macro_expand_impl(args[0], *THE_LISP_VM);
}

DEFUN("%MACRO-EXPAND1", func_macro_expand1, g.kernel(), false)
{
    /***
        (macro-expand1 expr)
    */
    CHECK_NARGS_EXACTLY(1);
    const bool just_one = true;
    return macro_expand_impl(args[0], *THE_LISP_VM, just_one);
}

DEFUN("%EVAL", func_eval, g.kernel(), false)
{
    /***
        (eval expr)
    */
    auto vm = THE_LISP_VM;
    auto save = vm->save();

    CHECK_NARGS_EXACTLY(1);
    auto expr = args[0];
    auto expr_handle = gc.pin_value(expr);

    try
    {
        bytecode::Emitter e(compiler::THE_ROOT_SCOPE);

        auto expanded = macro_expand_impl(expr, *vm);
        gc.unpin_value(expr_handle);

        compiler::compile(e, expanded, true, false);
        e.emit_halt();
        e.lock();

        g.resize_globals(compiler::THE_ROOT_SCOPE->locals().size());

        vm->push_frame(nullptr, 0);

        vm->execute(e.bytecode().data());
    }
    catch (VM_State::Signal_Exception e)
    {
        vm->restore(save);
        raised_signal = true;
        return e.what;
    }

    auto result = vm->pop_param();
    vm->restore(save);
    return result;
}

DEFUN("%GENSYM", func_gensym, g.kernel(), false)
{
    /***
        (gensym &optional hint)
    */
    static unsigned int counter = 0;
    std::string sym_name;
    if (nargs != 0)
    {
        auto hint = args[0];
        CHECK_STRING(hint);
        sym_name = lisp_string_to_native_string(hint);
    }
    else
    {
        sym_name = "G";
    }

    sym_name += std::to_string(counter++);
    return gc.alloc_object<Symbol>(sym_name);
}

DEFUN("%MAKE-SYMBOL", func_make_symbol, g.kernel(), false)
{
    /***
        (make-symbol symbol-name)
    */
    CHECK_NARGS_EXACTLY(1);
    std::string name;
    CHECK_STRING(args[0]);
    auto array = args[0].as_object()->simple_array();
    for (Fixnum i = 0; i < array->size(); ++i)
    {
        auto codepoint = array->at(i).as_character();
        name += reinterpret_cast<const char*>(&codepoint);
    }
    return gc.alloc_object<Symbol>(name);
}

DEFUN("%SYMBOL-NAME", func_symbol_name, g.kernel(), false)
{
    /***
        (symbol-name symbol)
    */
    CHECK_NARGS_EXACTLY(1);
    CHECK_SYMBOL(args[0]);
    return gc.alloc_string(args[0].as_object()->symbol()->name());
}

DEFUN("%SYMBOL-PACKAGE", func_symbol_package, g.kernel(), false)
{
    /***
        (symbol-name symbol)
    */
    CHECK_NARGS_EXACTLY(1);
    CHECK_SYMBOL(args[0]);
    auto pkg = args[0].as_object()->symbol()->package();
    return pkg ? pkg->as_lisp_value() : Value::nil();
}

DEFUN("%EXPORT", func_export, g.kernel(), false)
{
    CHECK_NARGS_EXACTLY(2);
    auto package = g.packages.current();
    {
        Value res;
        raised_signal = check_package(args[1], &package, res);
        if (raised_signal)
        {
            return res;
        }
    }

    if (package == nullptr)
    {
        raised_signal = true;
        GC_GUARD();
        auto res = gc.list(g.s_SIMPLE_ERROR, gc.alloc_string("Package does not exist"), args[1]);
        GC_UNGUARD();
        return res;
    }

    auto list_of_symbols = args[0];
    CHECK_LIST(list_of_symbols);
    while (!list_of_symbols.is_nil())
    {
        auto sym = car(list_of_symbols);
        CHECK_SYMBOL(sym);
        package->export_symbol(sym.as_object()->symbol()->name());
        list_of_symbols = cdr(list_of_symbols);
    }
    return g.s_T;
}

DEFUN("%IMPORT", func_import, g.kernel(), false)
{
    CHECK_NARGS_EXACTLY(2);
    Package *package = nullptr;
    {
        Value res;
        raised_signal = check_package(args[1], &package, res);
        if (raised_signal)
        {
            return res;
        }
    }

    if (package == nullptr)
    {
        raised_signal = true;
        GC_GUARD();
        auto res = gc.list(g.s_SIMPLE_ERROR, gc.alloc_string("Package does not exist"), args[1]);
        GC_UNGUARD();
        return res;
    }

    auto list_of_symbols = args[0];
    CHECK_LIST(list_of_symbols);
    while (!list_of_symbols.is_nil())
    {
        auto sym = car(list_of_symbols);
        CHECK_SYMBOL(sym);
        package->import_symbol(sym);
        list_of_symbols = cdr(list_of_symbols);
    }
    return g.s_T;
}

DEFUN("%INTERN", func_intern, g.kernel(), false)
{
    /***
        (intern symbol-name &optional package)
    */
    CHECK_NARGS_EXACTLY(2);
    std::string name;
    CHECK_STRING(args[0]);
    Package *package = nullptr;

    {
        Value res;
        raised_signal = check_package(args[1], &package, res);
        if (raised_signal)
        {
            return res;
        }
    }

    if (package == nullptr)
    {
        raised_signal = true;
        GC_GUARD();
        auto res = gc.list(g.s_SIMPLE_ERROR, gc.alloc_string("Package does not exist"), args[1]);
        GC_UNGUARD();
        return res;
    }

    auto array = args[0].as_object()->simple_array();
    for (Fixnum i = 0; i < array->size(); ++i)
    {
        auto codepoint = array->at(i).as_character();
        name += reinterpret_cast<const char*>(&codepoint);
    }

    auto sym = package->intern_symbol(name);
    return sym;
}

DEFUN("%FIND-SYMBOL", func_find_symbol, g.kernel(), false)
{
    CHECK_NARGS_EXACTLY(2);
    Package *package = nullptr;
    {
        Value res;
        raised_signal = check_package(args[1], &package, res);
        if (raised_signal)
        {
            return res;
        }
    }

    if (package == nullptr)
    {
        raised_signal = true;
        GC_GUARD();
        auto res = gc.list(g.s_SIMPLE_ERROR, gc.alloc_string("Package does not exist"), args[1]);
        GC_UNGUARD();
        return res;
    }

    CHECK_STRING(args[0]);
    auto str = lisp_string_to_native_string(args[0]);
    Package::Symbol_Location_Type location;
    Value symbol;
    if (package->find_symbol(str, symbol, &location))
    {
        Value status;
        switch (location)
        {
            case Package::Symbol_Location_Type::Internal: status = g.get_keyword("INTERNAL"); break;
            case Package::Symbol_Location_Type::External: status = g.get_keyword("EXTERNAL"); break;
            case Package::Symbol_Location_Type::Inherited: status = g.get_keyword("INHERITED"); break;
        }
        return gc.list(symbol, status);
    }
    return gc.list(Value::nil(), Value::nil());
}


DEFUN("%EXIT", func_exit, g.kernel(), false)
{
    /***
        (exit &optional n)
    */
    int code = 0;
    if (nargs != 0)
    {
        CHECK_FIXNUM(args[0]);
        code = args[0].as_fixnum();
    }
    exit(code);
}

DEFUN("%SIGNAL", func_signal, g.kernel(), false)
{
    /***
        (signal tag &rest args)
    */
    CHECK_NARGS_AT_LEAST(1);
    raised_signal = true;
    return to_list(args, nargs);
}

DEFUN("%MAKE-ARRAY", func_make_array, g.kernel(), false)
{
    /***
        (make-array length type fill-pointer)
    */
    CHECK_NARGS_EXACTLY(3);
    auto length = args[0];
    CHECK_FIXNUM(length);
    auto fill_pointer = args[2];
    CHECK_FIXNUM(fill_pointer);
    auto type = args[1];
    if (type == g.s_CHARACTER || type == g.s_FIXNUM)
    {
        return gc.alloc_object<Simple_Array>(type, length.as_fixnum(), fill_pointer.as_fixnum());
    }
    return gc.alloc_object<Simple_Array>(g.s_T, length.as_fixnum(), fill_pointer.as_fixnum());
}

DEFUN("%ARRAY-PUSH-BACK", func_array_push_back, g.kernel(), false)
{
    /***
        (array-push-back array value)
    */
    CHECK_NARGS_EXACTLY(2);
    auto array_val = args[0];
    CHECK_SIMPLE_ARRAY(array_val);

    auto value = args[1];
    auto array = array_val.as_object()->simple_array();
    auto type = array->element_type();
    if (type != g.s_T)
    {
        if (type == g.s_FIXNUM)
        {
            CHECK_FIXNUM(value);
        }
        else if (type == g.s_CHARACTER)
        {
            CHECK_CHARACTER(value);
        }
    }
    array->push_back(value);
    return value;
}

DEFUN("%ARRAY-CAPACITY", func_array_capacity, g.kernel(), false)
{
    /***
        (array-capacity array)
    */
    CHECK_NARGS_EXACTLY(1);

    auto array = args[0];
    CHECK_SIMPLE_ARRAY(array);
    return Value::wrap_fixnum(array.as_object()->simple_array()->capacity());
}

DEFUN("%ARRAY-LENGTH", func_array_length, g.kernel(), false)
{
    /***
        (array-length array)
    */
    CHECK_NARGS_EXACTLY(1);

    auto array = args[0];
    CHECK_SIMPLE_ARRAY(array);
    return Value::wrap_fixnum(array.as_object()->simple_array()->size());
}

DEFUN("%ARRAY-TYPE", func_array_type, g.kernel(), false)
{
    /***
        (array-type array)
    */
    CHECK_NARGS_EXACTLY(1);

    auto array = args[0];
    CHECK_SIMPLE_ARRAY(array);
    return array.as_object()->simple_array()->element_type();
}

DEFUN("%BITS-OF", func_bits_of, g.kernel(), false)
{
    /***
        (bits-of object)
    */
    CHECK_NARGS_EXACTLY(1);

    auto obj = args[0];
    auto ret = gc.alloc_object<Simple_Array>(g.s_BIT, 64);
    auto bits = obj.bits();
    auto array = ret.as_object()->simple_array();
    for (int i = 0; i < 64; ++i)
    {
        array->at(i) = Value::wrap_fixnum(bits & 1);
        bits >>= 1;
    }
    return ret;
}

DEFUN("%CODE-CHAR", func_code_char, g.kernel(), false)
{
    /***
        (code-char integer)
    */
    CHECK_NARGS_EXACTLY(1);
    CHECK_FIXNUM(args[0]);
    auto char_code = args[0].as_fixnum();
    return Value::wrap_character(char_code);
}

DEFUN("%CHAR-CODE", func_char_code, g.kernel(), false)
{
    /***
        (char-code character)
    */
    CHECK_NARGS_EXACTLY(1);
    CHECK_CHARACTER(args[0]);
    auto character = args[0].as_character();
    return Value::wrap_fixnum(character);
}

static
std::ios_base::openmode get_mode(Value v)
{
    if (v == g.s_OVERWRITE)
    {
        return std::ios_base::out | std::ios_base::trunc;
    }
    else if (v == g.s_READ)
    {
        return std::ios_base::in;
    }
    else if (v == g.s_APPEND)
    {
        return std::ios_base::out | std::ios_base::app;
    }
    return static_cast<std::ios_base::openmode>(0);
}

DEFUN("%OPEN", func_open, g.kernel(), false)
{
    /***
        (open file-path direction)
    */
    CHECK_NARGS_EXACTLY(2);

    CHECK_STRING(args[0]);

    auto path = lisp_string_to_native_string(args[0]);
    auto direction = args[1];
    auto mode = std::ios_base::binary;
    if (direction.is_cons())
    {
        auto p = direction;
        while (!p.is_nil())
        {
            CHECK_SYMBOL(car(p));
            mode |= get_mode(car(p));
            p = cdr(p);
        }
    }
    else
    {
        CHECK_SYMBOL(direction);
        mode = get_mode(direction);
    }
    if (mode != std::ios_base::binary)
    {
        return gc.alloc_object<File_Stream>(path, mode);
    }
    return Value::nil();
}

DEFUN("%CLOSE", func_close, g.kernel(), false)
{
    /***
        (close file-stream)
    */
    CHECK_NARGS_EXACTLY(1);
    auto it = args[0];
    CHECK_FILE_STREAM(it);
    it.as_object()->file_stream()->stream().close();
    return g.s_T;
}

DEFUN("%FILE-PATH", func_file_path, g.kernel(), false)
{
    /***
        (file-path file-stream)
    */
    CHECK_NARGS_EXACTLY(1);
    auto it = args[0];
    return gc.alloc_string(it.as_object()->file_stream()->path());
}

DEFUN("%FILE-OK-P", func_file_ok_p, g.kernel(), false)
{
    /***
        (file-ok-p file-stream)
    */
    CHECK_NARGS_EXACTLY(1);
    auto it = args[0];
    if (it.is_nil()) return it;
    CHECK_FILE_STREAM(it);
    return it.as_object()->file_stream()->stream().good() ? g.s_T : Value::nil();
}

DEFUN("%FILE-EOF-P", func_file_eof_p, g.kernel(), false)
{
    /***
        (file-eof-p file-stream)
    */
    CHECK_NARGS_EXACTLY(1);
    auto it = args[0];
    if (it.is_nil()) return it;
    CHECK_FILE_STREAM(it);
    return it.as_object()->file_stream()->stream().eof() ? g.s_T : Value::nil();
}

DEFUN("%FILE-MODE", func_file_mode, g.kernel(), false)
{
    /***
        (file-mode file-stream)
    */
    CHECK_NARGS_EXACTLY(1);
    auto it = args[0];
    CHECK_FILE_STREAM(it);
    auto mode = it.as_object()->file_stream()->mode();
    std::vector<Value> vals;
    if (mode & std::ios_base::app)
    {
        vals.push_back(g.s_APPEND);
    }
    if (mode & std::ios_base::trunc)
    {
        vals.push_back(g.s_OVERWRITE);
    }
    if (mode & std::ios_base::in)
    {
        vals.push_back(g.s_READ);
    }
    return to_list(vals);
}

DEFUN("%FILE-FLUSH", func_file_flush, g.kernel(), false)
{
    /***
        (file-flush file-stream)
    */
    CHECK_NARGS_EXACTLY(1);
    auto it = args[0];
    CHECK_FILE_STREAM(it);
    it.as_object()->file_stream()->stream().flush();
    return g.s_T;
}

DEFUN("%FILE-READ-BYTE", func_file_read_byte, g.kernel(), false)
{
    /***
        (file-read-byte file-stream)
    */
    CHECK_NARGS_EXACTLY(1);
    auto it = args[0];
    CHECK_FILE_STREAM(it);
    auto &stm = it.as_object()->file_stream()->stream();
    if (stm.eof())
    {
        raised_signal = true;
        return gc.list(g.s_END_OF_FILE);
    }
    return Value::wrap_fixnum(stm.get());
}

DEFUN("%FILE-PEEK-BYTE", func_file_peek_byte, g.kernel(), false)
{
    /***
        (file-peek-byte file-stream)
    */
    CHECK_NARGS_EXACTLY(1);
    auto it = args[0];
    CHECK_FILE_STREAM(it);
    auto &stm = it.as_object()->file_stream()->stream();
    if (stm.eof())
    {
        raised_signal = true;
        return gc.list(g.s_END_OF_FILE);
    }
    return Value::wrap_fixnum(stm.peek());
}

DEFUN("%FILE-PEEK-CHARACTER", func_file_peek_character, g.kernel(), false)
{
    /***
        (file-peek-character file-stream)
    */
    CHECK_NARGS_EXACTLY(1);
    auto it = args[0];
    CHECK_FILE_STREAM(it);
    auto fs = it.as_object()->file_stream();
    if (fs->stream().eof())
    {
        raised_signal = true;
        return gc.list(g.s_END_OF_FILE);
    }
    return Value::wrap_character(fs->peek_character());
}

DEFUN("%FILE-READ-CHARACTER", func_file_read_character, g.kernel(), false)
{
    /***
        (file-read-character file-stream)
    */
    CHECK_NARGS_EXACTLY(1);
    auto it = args[0];
    CHECK_FILE_STREAM(it);
    auto fs = it.as_object()->file_stream();
    if (fs->stream().eof())
    {
        raised_signal = true;
        return gc.list(g.s_END_OF_FILE);
    }
    return Value::wrap_character(fs->read_character());
}

DEFUN("%FUNCTION-DEFINITION", func_function_definition, g.kernel(), false)
{
    /***
        (function-definition symbol)
    */
    CHECK_NARGS_EXACTLY(1);
    auto sym = args[0];
    CHECK_SYMBOL(sym);
    return sym.as_object()->symbol()->function();
}

DEFUN("%STRUCTURE-DEFINITION", func_structure_definition, g.kernel(), false)
{
    /***
        (structure-definition object)
     */
    CHECK_NARGS_EXACTLY(1);
    CHECK_STRUCT(args[0]);
    return args[0].as_object()->structure()->type();
}

DEFUN("%CREATE-INSTANCE", func_create_instance, g.kernel(), false)
{
    /***
        (create-instance type &rest slots)
     */
    CHECK_NARGS_AT_LEAST(1);
    auto instance_val = gc.alloc_object<Structure>(args[0], nargs-1);
    if (nargs > 1)
    {
        auto inst = instance_val.as_object()->structure();
        for (uint32_t i = 1; i < nargs; ++i)
        {
            inst->slot_value(i-1) = args[i];
        }
    }
    return instance_val;
}

DEFUN("%GET-SLOT", func_get_slot, g.kernel(), false)
{
    /***
        (get-slot object n)
     */
    CHECK_NARGS_EXACTLY(2);
    CHECK_STRUCT(args[0]);
    CHECK_FIXNUM(args[1]);
    auto index = args[1].as_fixnum();
    return args[0].as_object()->structure()->slot_value(index);
}

DEFUN("%SET-SLOT", func_set_slot, g.kernel(), false)
{
    /***
        (set-slot object n value)
     */
    CHECK_NARGS_EXACTLY(3);
    CHECK_STRUCT(args[0]);
    CHECK_FIXNUM(args[1]);
    auto index = args[1].as_fixnum();
    auto value = args[2];
    args[0].as_object()->structure()->slot_value(index) = value;
    return value;
}

DEFUN("%GC-PAUSE", func_gc_pause, g.kernel(), false)
{
    CHECK_NARGS_EXACTLY(0);
    return gc.pause() ? g.s_T : Value::nil();
}

DEFUN("%GC-PAUSED-P", func_gc_paused_p, g.kernel(), false)
{
    CHECK_NARGS_EXACTLY(0);
    return gc.paused() ? g.s_T : Value::nil();
}

DEFUN("%GC-SET-PAUSED", func_gc_set_paused, g.kernel(), false)
{
    CHECK_NARGS_EXACTLY(1);
    gc.set_paused(!args[0].is_nil());
    return args[0];
}

DEFUN("%GC-COLLECT", func_gc_collect, g.kernel(), false)
{
    return Value::wrap_fixnum(gc.mark_and_sweep());
}

DEFUN("%GC-GET-CONSED", func_gc_get_consed, g.kernel(), false)
{
    return Value::wrap_fixnum(gc.get_consed());
}

DEFUN("%GC-GET-FREED", func_gc_get_freed, g.kernel(), false)
{
    return Value::wrap_fixnum(gc.get_freed());
}

DEFUN("%GC-GET-TIME-SPENT-IN-GC", func_gc_get_time_spent_in_gc, g.kernel(), false)
{
    return Value::wrap_fixnum(gc.get_time_spent_in_gc());
}

DEFUN("%GC-GET-TIMES-GC-HAS-RUN", func_gc_get_times_gc_has_run, g.kernel(), false)
{
    return Value::wrap_fixnum(gc.get_times_gc_has_run());
}

DEFUN("%GC-GET-COLLECT-THRESHOLD", func_gc_get_collect_threshold, g.kernel(), false)
{
    return Value::wrap_fixnum(gc.get_collect_threshold());
}

DEFUN("%GC-SET-COLLECT-THRESHOLD", func_gc_set_collect_threshold, g.kernel(), false)
{
    CHECK_NARGS_EXACTLY(1);
    CHECK_FIXNUM(args[0]);
    gc.set_collect_threshold(args[0].as_fixnum());
    return Value::nil();
}

///////////////////////////////////////////////////////////////////////
// Exported Functions

DEFUN("BIT-NOT", func_bit_not, g.kernel(), true)
{
    /***
        (bit-not x)
    */
    CHECK_NARGS_EXACTLY(1);
    CHECK_FIXNUM(args[0]);
    auto x = args[0].as_fixnum();
    return Value::wrap_fixnum(~x);
}

DEFUN("BIT-AND", func_bit_and, g.kernel(), true)
{
    /***
        (bit-and x y)
    */
    CHECK_NARGS_EXACTLY(2);
    CHECK_FIXNUM(args[0]);
    CHECK_FIXNUM(args[1]);
    auto x = args[0].as_fixnum();
    auto y = args[1].as_fixnum();
    return Value::wrap_fixnum(x & y);
}

DEFUN("BIT-IOR", func_bit_ior, g.kernel(), true)
{
    /***
        (bit-or x y &rest z)
    */
    CHECK_NARGS_AT_LEAST(2);
    CHECK_FIXNUM(args[0]);
    CHECK_FIXNUM(args[1]);

    auto res = args[0] | args[1];
    for (uint32_t i = 2; i < nargs; ++i)
    {
        CHECK_FIXNUM(args[i]);
        res |= args[i];
    }
    return res;
}

DEFUN("BIT-XOR", func_bit_xor, g.kernel(), true)
{
    /***
        (bit-xor x y)
    */
    CHECK_NARGS_EXACTLY(2);
    CHECK_FIXNUM(args[0]);
    CHECK_FIXNUM(args[1]);
    auto x = args[0].as_fixnum();
    auto y = args[1].as_fixnum();
    return Value::wrap_fixnum(x ^ y);
}

DEFUN("BIT-SHIFT", func_bit_shift, g.kernel(), true)
{
    /***
        (bit-shift integer count)
    */
    CHECK_NARGS_EXACTLY(2);
    CHECK_FIXNUM(args[0]);
    CHECK_FIXNUM(args[1]);
    auto integer = args[0].as_fixnum();
    auto count = args[1].as_fixnum();
    if (count > 0)
    {
        return Value::wrap_fixnum(integer << count);
    }
    return Value::wrap_fixnum(integer >> -count);
}

DEFUN("GET-WORKING-DIRECTORY", func_get_working_directory, g.core(), true)
{
    /***
        (get-working-directory)
    */
    std::error_code error;
    auto current_path = plat::get_working_directory(error);
    return error.value() != 0 ? Value::nil() : gc.alloc_string(current_path);
}

DEFUN("CHANGE-DIRECTORY", func_change_directory, g.core(), true)
{
    /***
        (change-directory path)
    */
    CHECK_NARGS_EXACTLY(1);
    CHECK_STRING(args[0]);
    auto new_path = lisp_string_to_native_string(args[0]);
    std::error_code error;
    plat::change_directory(new_path, error);
    if (error.value() != 0)
    {
        return Value::nil();
    }

    error.clear();
    auto current_path = plat::get_working_directory(error);
    return error.value() != 0 ? Value::nil() : gc.alloc_string(current_path);
}

DEFUN("GET-EXECUTABLE-PATH", func_get_executable_path, g.core(), true)
{
    /***
        (get-executable-path)
    */
    CHECK_NARGS_EXACTLY(0);
    return gc.alloc_string(plat::get_executable_path());
}

DEFUN("GET-CLOCK-TICKS", func_get_clock_ticks, g.core(), true)
{
    /***
        (get-clock-ticks)
    */
    CHECK_NARGS_EXACTLY(0);
    auto now = std::chrono::high_resolution_clock::now();
    auto duration = now.time_since_epoch();
    auto microseconds = std::chrono::duration_cast<std::chrono::microseconds>(duration);
    return Value::wrap_fixnum(microseconds.count());
}

DEFUN("CLOCKS-PER-SECOND", func_clocks_per_second, g.core(), true)
{
    /***
        (clocks-per-second)
    */
    CHECK_NARGS_EXACTLY(0);
    return Value::wrap_fixnum(1000000); // @FIXME use something better than costant number
}

DEFUN("OPERATING-SYSTEM", func_operating_system, g.core(), true)
{
    /***
        (operating-system)
    */
#if defined(_WIN32) || defined(_WIN64)
    return g.core()->export_symbol("WINDOWS");
#elif defined(__linux__)
    return g.core()->export_symbol("LINUX");
#elif defined(__APPLE__) || defined(__MACH__)
    return g.core()->export_symbol("MAC-OS-X");
#else
    return g.core()->export_symbol("UNKNOWN-OS");
#endif
}

static
bool ffi_try_marshal(Value val, void **out_ptr)
{
    if (val.is_type(Object_Type::System_Pointer))
    {
        *out_ptr = val.as_object()->system_pointer();
        return true;
    }
    if (val.is_fixnum())
    {
        *out_ptr = reinterpret_cast<void*>(val.as_fixnum());
        return true;
    }
    if (val.is_character())
    {
        *out_ptr = reinterpret_cast<void*>(val.as_character());
        return true;
    }
    if (val.is_object())
    {
        if (val.is_type(Object_Type::Simple_Array))
        {
            auto array = val.as_object()->simple_array();
            if (array->element_type() == g.s_CHARACTER)
            {
                auto str = lisp_string_to_native_string(val);
                auto buffer = (char*)ffi::alloc_mem(str.size()+1); // @LEAK
                memcpy(buffer, str.data(), str.size());
                buffer[str.size()] = 0;
                *out_ptr = buffer;
                return true;
            }
        }
    }
    return false;
}

DEFUN("ERRNO", func_errno, g.kernel(), true)
{
    /***
        (errno)
     */
    return Value::wrap_fixnum(errno);
}

DEFUN("ERRNO-STR", func_errno_str, g.kernel(), true)
{
    /***
        (errno-str)
     */
    if (nargs != 0)
    {
        CHECK_FIXNUM(args[0]);
        return gc.alloc_string(strerror(args[0].as_fixnum()));
    }
    return gc.alloc_string(strerror(errno));
}

DEFUN("FFI-MACHINE-POINTER-SIZE", func_ffi_machine_pointer_size, g.kernel(), true)
{
    /***
        (ffi-machine-pointer-size)
     */
    return Value::wrap_fixnum(sizeof(void*));
}

DEFUN("FFI-OPEN", func_ffi_open, g.kernel(), true)
{
    /***
        (ffi-open dll-path)
     */
    CHECK_NARGS_EXACTLY(1);
    auto lib = args[0];
    CHECK_STRING(lib);
    auto lib_str = lisp_string_to_native_string(lib);
    auto handle = ffi::open(lib_str.c_str());
    return handle ? gc.alloc_object<System_Pointer>(handle) : Value::nil();
}

DEFUN("FFI-CLOSE", func_ffi_close, g.kernel(), true)
{
    /***
        (ffi-close dll-handle)
     */
    CHECK_NARGS_EXACTLY(1);
    CHECK_SYSTEM_POINTER(args[0]);
    ffi::close(args[0].as_object()->system_pointer());
    return Value::nil();
}

DEFUN("FFI-GET-SYMBOL", func_ffi_get_symbol, g.kernel(), true)
{
    /***
        (ffi-get-symbol dll-handle symbol-name)
     */
    CHECK_NARGS_EXACTLY(2);
    CHECK_SYSTEM_POINTER(args[0]);
    CHECK_STRING(args[1]);

    auto handle = args[0].as_object()->system_pointer();
    auto symbol = args[1];
    auto symbol_str = lisp_string_to_native_string(symbol);

    auto ptr = ffi::getsym(handle, symbol_str.c_str());
    return ptr ? gc.alloc_object<System_Pointer>(ptr) : Value::nil();
}

DEFUN("FFI-CALL", func_ffi_call, g.kernel(), true)
{
    /***
        (ffi-call c-function &rest args)
     */
    CHECK_NARGS_AT_LEAST(1);
    CHECK_SYSTEM_POINTER(args[0]);
    auto func = args[0].as_object()->system_pointer();
    std::vector<void *> marshalled;
    for (uint32_t i = 1; i < nargs; ++i)
    {
        void *m = nullptr;
        if (ffi_try_marshal(args[i], &m))
        {
            marshalled.push_back(m);
        }
        else
        {
            raised_signal = true;
            GC_GUARD();
            auto res = gc.list(g.s_MARSHAL_ERROR, gc.alloc_string("Cannot marshal object"), args[i]);
            GC_UNGUARD();
            return res;
        }
    }
    auto result = ffi::call(func, marshalled.data(), marshalled.size());
    return gc.alloc_object<System_Pointer>(result);
}

DEFUN("FFI-NULLPTR", func_ffi_nullptr, g.kernel(), true)
{
    /***
        (ffi-nullptr)
     */
    CHECK_NARGS_EXACTLY(0);
    return gc.alloc_object<System_Pointer>(nullptr);
}

DEFUN("FFI-ALLOC", func_ffi_alloc, g.kernel(), true)
{
    /***
        (ffi-alloc size)
     */
    CHECK_NARGS_EXACTLY(1);
    CHECK_FIXNUM(args[0]);
    auto size = args[0].as_fixnum();
    return gc.alloc_object<System_Pointer>(ffi::alloc_mem(size));
}

DEFUN("FFI-ZERO-ALLOC", func_ffi_zero_alloc, g.kernel(), true)
{
    /***
        (ffi-zero-alloc size)
     */
    CHECK_NARGS_EXACTLY(1);
    CHECK_FIXNUM(args[0]);
    auto size = args[0].as_fixnum();
    return gc.alloc_object<System_Pointer>(ffi::calloc_mem(size));
}

DEFUN("FFI-FREE", func_ffi_free, g.kernel(), true)
{
    /***
        (ffi-free pointer)
     */
    CHECK_NARGS_EXACTLY(1);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = args[0].as_object()->system_pointer();
    ffi::free_mem(ptr);
    args[0].as_object()->system_pointer(nullptr);
    return g.s_T;
}

DEFUN("FFI-REF", func_ffi_ref, g.kernel(), true)
{
    /***
        (ffi-ref pointer &optional offset)
     */
    CHECK_NARGS_AT_LEAST(1);
    CHECK_SYSTEM_POINTER(args[0]);
    if (nargs == 1)
    {
        return gc.alloc_object<System_Pointer>(args[0].as_object()->pointer_ref());
    }
    else
    {
        auto ptr = reinterpret_cast<uint8_t*>(args[0].as_object()->system_pointer());
        CHECK_FIXNUM(args[1]);
        auto offset = args[1].as_fixnum();

        return gc.alloc_object<System_Pointer>(ptr + offset);
    }
}

DEFUN("FFI-REF-8", func_ffi_ref_8, g.kernel(), true)
{
    /***
        (ffi-ref-8 pointer)
     */
    CHECK_NARGS_EXACTLY(1);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = reinterpret_cast<uint8_t*>(args[0].as_object()->system_pointer());
    return Value::wrap_fixnum(*ptr);
}

DEFUN("FFI-REF-16", func_ffi_ref_16, g.kernel(), true)
{
    /***
        (ffi-ref-16 pointer)
     */
    CHECK_NARGS_EXACTLY(1);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = reinterpret_cast<uint16_t*>(args[0].as_object()->system_pointer());
    return Value::wrap_fixnum(*ptr);
}

DEFUN("FFI-REF-32", func_ffi_ref_32, g.kernel(), true)
{
    /***
        (ffi-ref-32 pointer)
     */
    CHECK_NARGS_EXACTLY(1);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = reinterpret_cast<uint32_t*>(args[0].as_object()->system_pointer());
    return Value::wrap_fixnum(*ptr);
}

DEFUN("FFI-REF-64", func_ffi_ref_64, g.kernel(), true)
{
    /***
        (ffi-ref-64 pointer)
     */
    CHECK_NARGS_EXACTLY(1);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = reinterpret_cast<uint64_t*>(args[0].as_object()->system_pointer());
    return Value::wrap_fixnum(*ptr);
}

DEFUN("FFI-SET-REF", func_ffi_set_ref, g.kernel(), true)
{
    /***
        (ffi-set-ref pointer value value-size)
     */
    CHECK_NARGS_EXACTLY(3);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = reinterpret_cast<uint8_t*>(args[0].as_object()->system_pointer());
    CHECK_SYSTEM_POINTER(args[1]);
    auto value = reinterpret_cast<uint8_t*>(args[1].as_object()->system_pointer());
    CHECK_FIXNUM(args[2]);
    auto value_size = args[2].as_fixnum();

    memcpy(ptr, value, value_size);
    return args[2];
}

DEFUN("FFI-SET-REF-8", func_ffi_set_ref_8, g.kernel(), true)
{
    /***
        (ffi-set-ref-8 pointer value)
     */
    CHECK_NARGS_EXACTLY(2);
    CHECK_SYSTEM_POINTER(args[0]);
    uint8_t value;
    auto ptr = reinterpret_cast<decltype(value)*>(args[0].as_object()->system_pointer());
    if (args[1].is_type(Object_Type::System_Pointer))
    {
        value = static_cast<decltype(value)>(
            reinterpret_cast<uintptr_t>(args[1].as_object()->system_pointer()));
    }
    else
    {
        CHECK_FIXNUM(args[1]);
        value = args[1].as_fixnum() & 0xff;
    }
    *ptr = value;
    return Value::wrap_fixnum(value);
}

DEFUN("FFI-SET-REF-16", func_ffi_set_16, g.kernel(), true)
{
    /***
        (ffi-set-ref-16 pointer value)
     */
    CHECK_NARGS_EXACTLY(2);
    CHECK_SYSTEM_POINTER(args[0]);
    uint16_t value;
    auto ptr = reinterpret_cast<decltype(value)*>(args[0].as_object()->system_pointer());
    if (args[1].is_type(Object_Type::System_Pointer))
    {
        value = static_cast<decltype(value)>(
            reinterpret_cast<uintptr_t>(args[1].as_object()->system_pointer()));
    }
    else
    {
        CHECK_FIXNUM(args[1]);
        value = args[1].as_fixnum() & 0xffff;
    }
    *ptr = value;
    return Value::wrap_fixnum(value);
}

DEFUN("FFI-SET-REF-32", func_ffi_set_32, g.kernel(), true)
{
    /***
        (ffi-set-ref-32 pointer value)
     */
    CHECK_NARGS_EXACTLY(2);
    CHECK_SYSTEM_POINTER(args[0]);
    uint32_t value;
    auto ptr = reinterpret_cast<decltype(value)*>(args[0].as_object()->system_pointer());
    if (args[1].is_type(Object_Type::System_Pointer))
    {
        value = static_cast<decltype(value)>(
            reinterpret_cast<uintptr_t>(args[1].as_object()->system_pointer()));
    }
    else
    {
        CHECK_FIXNUM(args[1]);
        value = args[1].as_fixnum() & 0xffffffff;
    }
    *ptr = value;
    return Value::wrap_fixnum(value);
}

DEFUN("FFI-SET-REF-64", func_ffi_set_64, g.kernel(), true)
{
    /***
        (ffi-set-ref-64 pointer value)
     */
    CHECK_NARGS_EXACTLY(2);
    CHECK_SYSTEM_POINTER(args[0]);
    uint64_t value;
    auto ptr = reinterpret_cast<decltype(value)*>(args[0].as_object()->system_pointer());
    if (args[1].is_type(Object_Type::System_Pointer))
    {
        value = static_cast<decltype(value)>(
            reinterpret_cast<uintptr_t>(args[1].as_object()->system_pointer()));
    }
    else
    {
        CHECK_FIXNUM(args[1]);
        value = args[1].as_fixnum();
    }
    *ptr = value;
    return Value::wrap_fixnum(value);
}

DEFUN("FFI-MARSHAL", func_ffi_marshal, g.kernel(), true)
{
    /***
        (ffi-marshal object)
     */
    CHECK_NARGS_EXACTLY(1);
    void *result = nullptr;
    if (ffi_try_marshal(args[0], &result))
    {
        return gc.alloc_object<System_Pointer>(result);
    }
    raised_signal = true;
    GC_GUARD();
    auto res = gc.list(g.s_MARSHAL_ERROR, gc.alloc_string("Cannot marshal object"), args[0]);
    GC_UNGUARD();
    return res;
}

DEFUN("FFI-STRLEN", func_ffi_strlen, g.kernel(), true)
{
    CHECK_NARGS_EXACTLY(1);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = reinterpret_cast<const char*>(args[0].as_object()->system_pointer());
    return Value::wrap_fixnum(strlen(ptr));
}

DEFUN("FFI-COERCE-FIXNUM", func_ffi_coerce_fixnum, g.kernel(), true)
{
    /***
        (ffi-coerce-fixnum system-pointer)
     */
    CHECK_NARGS_EXACTLY(1);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = reinterpret_cast<Fixnum>(args[0].as_object()->system_pointer());
    return Value::wrap_fixnum(ptr);
}

DEFUN("FFI-COERCE-INT", func_ffi_coerce_int, g.kernel(), true)
{
    /***
        (ffi-coerce-int system-pointer)
     */
    CHECK_NARGS_EXACTLY(1);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = static_cast<int>(reinterpret_cast<uintptr_t>(args[0].as_object()->system_pointer()));
    return Value::wrap_fixnum(ptr);
}

DEFUN("FFI-COERCE-STRING", func_ffi_coerce_string, g.kernel(), true)
{
    /***
        (ffi-coerce-string system-pointer &optional length)
     */
    CHECK_NARGS_AT_LEAST(1);
    CHECK_SYSTEM_POINTER(args[0]);
    auto ptr = reinterpret_cast<const char*>(args[0].as_object()->system_pointer());
    if (nargs == 1)
    {
        return gc.alloc_string(ptr);
    }
    CHECK_FIXNUM(args[1]);
    auto len = args[1].as_fixnum();
    return gc.alloc_string(ptr, len);
}

}

static
void export_function(Package *package, const std::string &name, Primitive func)
{
    auto symbol = package->export_symbol(name);
    symbol.as_object()->symbol()->function(Value::wrap_primitive(func));
}

static
void internal_function(Package *package, const std::string &name, Primitive func)
{
    auto symbol = package->intern_symbol(name);
    symbol.as_object()->symbol()->function(Value::wrap_primitive(func));
}

static
void set_global(compiler::Scope *scope, Value symbol_value, Value value)
{
    if (!symbolp(symbol_value))
    {
        fprintf(stderr, "Tried to set global with non-symbol: %s\n", repr(symbol_value).c_str());
        return;
    }
    auto symbol = symbol_value.as_object()->symbol();
    auto root = scope->get_root();
    uint32_t global_idx;
    if (!root->resolve_local(symbol, global_idx))
    {
        root->create_variable(symbol, &global_idx);
    }
    g.resize_globals(root->locals().size());
    g.global_value_slots[global_idx] = value;
}

static
void initialize_globals(compiler::Scope *root_scope, char **argv)
{
    auto kernel = g.kernel();
    auto core = g.core();
    auto user = g.user();

    core->inherit(kernel);
    kernel->inherit(core);
    user->inherit(core);

    g.s_pplus            = kernel->intern_symbol("%+");
    g.s_pminus           = kernel->intern_symbol("%-");
    g.s_pCAR             = kernel->intern_symbol("%CAR");
    g.s_pCDR             = kernel->intern_symbol("%CDR");
    g.s_pCONS            = kernel->intern_symbol("%CONS");
    g.s_pEQ              = kernel->intern_symbol("%EQ");
    g.s_pRPLACA          = kernel->intern_symbol("%RPLACA");
    g.s_pRPLACD          = kernel->intern_symbol("%RPLACD");
    g.s_pSETQ            = kernel->intern_symbol("%SETQ");
    g.s_pAREF            = kernel->intern_symbol("%AREF");
    g.s_pASET            = kernel->intern_symbol("%ASET");
    g.s_pDEBUGGER        = kernel->intern_symbol("%DEBUGGER");
    g.s_pAPPLY           = kernel->intern_symbol("%APPLY");
    g.s_pTAGBODY         = kernel->intern_symbol("%TAGBODY");
    g.s_pGO              = kernel->intern_symbol("%GO");
    g.s_pSIGNAL          = kernel->intern_symbol("%SIGNAL");
    g.s_pHANDLER_CASE    = kernel->intern_symbol("%HANDLER-CASE");
    g.s_pDEFINE_MACRO    = kernel->intern_symbol("%DEFINE-MACRO");
    g.s_pLAMBDA          = kernel->intern_symbol("%LAMBDA");

    g.s_pFUNCALL         = kernel->export_symbol("FUNCALL");

    g.s_T                = core->export_symbol("T");
    g.s_IF               = core->export_symbol("IF");
    g.s_OR               = core->export_symbol("OR");
    g.s_FIXNUM           = core->export_symbol("FIXNUM");
    g.s_FLOAT            = core->export_symbol("FLOAT");
    g.s_CONS             = core->export_symbol("CONS");
    g.s_NULL             = core->export_symbol("NULL");
    g.s_LIST             = core->export_symbol("LIST");
    g.s_CHARACTER        = core->export_symbol("CHARACTER");
    g.s_SYMBOL           = core->export_symbol("SYMBOL");
    g.s_STRING           = core->export_symbol("STRING");
    g.s_FUNCTION         = core->export_symbol("FUNCTION");
    g.s_BOOLEAN          = core->export_symbol("BOOLEAN");
    g.s_STRUCTURE        = core->export_symbol("STRUCTURE");
    g.s_PACKAGE          = core->export_symbol("PACKAGE");
    g.s_FILE_STREAM      = core->export_symbol("FILE-STREAM");
    g.s_SYSTEM_POINTER   = core->export_symbol("SYSTEM-POINTER");
    g.s_SIMPLE_ARRAY     = core->export_symbol("SIMPLE-ARRAY");
    g.s_QUOTE            = core->export_symbol("QUOTE");
    g.s_QUASIQUOTE       = core->export_symbol("QUASIQUOTE");
    g.s_UNQUOTE          = core->export_symbol("UNQUOTE");
    g.s_UNQUOTE_SPLICING = core->export_symbol("UNQUOTE-SPLICING");
    g.s_TYPE_ERROR       = core->export_symbol("TYPE-ERROR");
    g.s_SIMPLE_ERROR     = core->export_symbol("SIMPLE-ERROR");
    g.s_aOPTIONAL        = core->export_symbol("&OPTIONAL");
    g.s_aREST            = core->export_symbol("&REST");
    g.s_aBODY            = core->export_symbol("&BODY");
    g.s_aWHOLE           = core->export_symbol("&WHOLE");

    g.s_DIVIDE_BY_ZERO_ERROR = core->export_symbol("DIVIDE-BY-ZERO-ERROR");
    g.s_INDEX_OUT_OF_BOUNDS_ERROR = core->export_symbol("INDEX-OUT-OF-BOUNDS-ERROR");
    g.s_END_OF_FILE      = core->export_symbol("END-OF-FILE");
    g.s_BIT              = core->export_symbol("BIT");

    g.s_OVERWRITE        = core->export_symbol("OVERWRITE");
    g.s_APPEND           = core->export_symbol("APPEND");
    g.s_READ             = core->export_symbol("READ");

    g.s_MARSHAL_ERROR    = core->export_symbol("MARSHAL-ERROR");

    g.s_BEGINNING        = core->export_symbol("BEGINNING");
    g.s_END              = core->export_symbol("END");
    g.s_CURRENT          = core->export_symbol("CURRENT");

    for (auto &init : g_function_initializers)
    {
        if (init.is_exported)
        {
            export_function(init.package, init.lisp_name, init.cpp_function);
        }
        else
        {
            internal_function(init.package, init.lisp_name, init.cpp_function);
        }
    }

#if defined(__unix__) || defined(__unix)
    set_global(root_scope,
               core->export_symbol("*STANDARD-OUTPUT*"),
               gc.alloc_object<File_Stream>("/dev/stdout", std::ios_base::binary | std::ios_base::app));
    set_global(root_scope,
               core->export_symbol("*STANDARD-ERROR*"),
               gc.alloc_object<File_Stream>("/dev/stderr", std::ios_base::binary | std::ios_base::app));
    set_global(root_scope,
               core->export_symbol("*STANDARD-INPUT*"),
               gc.alloc_object<File_Stream>("/dev/stdin", std::ios_base::binary | std::ios_base::in));
#else
    #error "Need to set stdio globals"
#endif
    set_global(root_scope,
               core->export_symbol("*PACKAGE*"),
               Value::nil());

    std::vector<Value> script_args;
    for(; *argv; ++argv)
    {
        script_args.push_back(gc.alloc_string(*argv));
    }

    set_global(root_scope,
               core->export_symbol("*COMMAND-LINE*"),
               to_list(script_args));

    {
        // call this function so the correct symbol is automatically exported
        bool b;
        primitives::func_operating_system(nullptr, 0, b);
    }

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
        m_stub.emitter = new bytecode::Emitter(nullptr);
        m_stub.function_offset = m_stub.emitter->position() + 1;
        m_stub.emitter->emit_push_literal(function);
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

template<typename Function, typename ...ExtraArgs>
static
Value map(Value list, Function func, ExtraArgs&&... args)
{
    if (list.is_nil())
    {
        return list;
    }

    GC_GUARD();
    auto head = gc.list(func(car(list), args...));
    auto current = head;
    list = cdr(list);
    while (!list.is_nil())
    {
        set_cdr(current, gc.list(func(car(list), args...)));
        current = cdr(current);
        list = cdr(list);
    }
    GC_UNGUARD();
    return head;
}

static
Value zip3(Value a, Value b, Value c)
{
    if (a.is_nil())
    {
        return a;
    }

    GC_GUARD();
    auto head = gc.list(gc.cons(car(a), gc.cons(car(b), car(c))));
    auto current = head;
    a = cdr(a); b = cdr(b); c = cdr(c);
    while (!a.is_nil())
    {
        auto next = gc.cons(car(a), gc.cons(car(b), car(c)));
        set_cdr(current, gc.list(next));
        current = cdr(current);
        a = cdr(a); b = cdr(b); c = cdr(c);
    }
    GC_UNGUARD();
    return head;
}

static
Value macro_expand_impl(Value obj, VM_State &vm, bool just_one)
{
    if (!obj.is_cons())
    {
        return obj;
    }
    auto car = first(obj);
    if (symbolp(car))
    {
        if (car == g.s_QUOTE)
        {
            return obj;
        }
        if (car == g.s_IF)
        {
            auto condition = macro_expand_impl(second(obj), vm);
            auto consequence = macro_expand_impl(third(obj), vm);
            auto alternative = macro_expand_impl(fourth(obj), vm);
            return gc.list(car, condition, consequence, alternative);
        }
        if (car == g.s_pDEFINE_MACRO)
        {
            auto macro_name = second(obj);
            auto params_list = third(obj);
            auto body = map(cdddr(obj), macro_expand_impl, vm, false);
            GC_GUARD();
            auto res = gc.cons(car, gc.cons(macro_name, gc.cons(params_list, body)));
            GC_UNGUARD();
            return res;
        }
        if (car == g.s_pLAMBDA)
        {
            auto lambda_list = second(obj); // @TODO: macroexpand &optional default expressions
            auto body = map(cddr(obj), macro_expand_impl, vm, false);
            GC_GUARD();
            auto res = gc.cons(car, gc.cons(lambda_list, body));
            GC_UNGUARD();
            return res;
        }
        if (car == g.s_pSETQ)
        {
            auto variable_name = second(obj);
            auto value = macro_expand_impl(third(obj), vm);
            return gc.list(car, variable_name, value);
        }
        if (car == g.s_pHANDLER_CASE)
        {
            auto form = macro_expand_impl(second(obj), vm);
            auto handlers = cddr(obj);
            auto handler_tags = map(handlers, first);
            auto handler_lambda_lists = map(handlers, second);
            auto handler_bodies = map(handlers, cddr);
            auto expanded_bodies = map(handler_bodies, macro_expand_impl, vm, false);
            auto expanded_handlers = zip3(handler_tags, handler_lambda_lists, expanded_bodies);
            GC_GUARD();
            auto res = gc.cons(car, gc.cons(form, expanded_handlers));
            GC_UNGUARD();
            return res;
        }
        auto it = g.macros.find(car.as_object()->symbol());
        if (it != g.macros.end())
        {
            auto func = it->second;
            auto function = func.as_object()->closure()->function();
            Value args = rest(obj);
            // @FIXME: This is a very ugly hack to get the &WHOLE form into a macro but works for testing
            if (function &&
                function->parameters().size() != 0 &&
                function->parameters()[0] == g.s_aWHOLE.as_object()->symbol())
            {
                GC_GUARD();
                args = gc.cons(obj, args);
                args = gc.cons(Value::nil(), args);
                GC_UNGUARD();
            }
            auto vec = to_vector(args);
            auto expand1 = vm.call_lisp_function(func, vec.data(), vec.size());
            if (expand1 == obj)
            {
                return expand1;
            }
            return just_one ? expand1 : macro_expand_impl(expand1, vm);
        }
    }
    return map(obj, macro_expand_impl, vm, false);
}


static FORCE_INLINE
bool is_whitespace(int c)
{
    return (c == ' ' || c == '\n' || c == '\r' || c == '\t');
}

static FORCE_INLINE
bool is_digit(int c)
{
    return ('0' <= c && c <= '9');
}

static FORCE_INLINE
bool is_symbol_start_char(int c)
{
    if (c == '(' || c == ')'
        || c == '\'' || c == '"' || c == '`' || c == ','
        || is_whitespace(c)
        || is_digit(c))
    {
        return false;
    }
    else
    {
        return true;
    }
}

static FORCE_INLINE
bool is_symbol_char(int c)
{
    return is_symbol_start_char(c) || is_digit(c);
}

static FORCE_INLINE
std::string str_upper(std::string in)
{
    std::transform(in.begin(), in.end(), in.begin(), [](unsigned char c){ return std::toupper(c) ; } );
    return in;
}

static FORCE_INLINE
int peek_consuming(std::istream &stream)
{
    while (is_whitespace(stream.peek()))
    {
        stream.get();
    }
    return stream.peek();
}

static
Value make_symbol(const std::string &str)
{
    auto package = g.packages.current();
    std::string symbol_name;
    std::string package_name;
    bool external = false;
    if (str.size() > 2 && str[0] == ':' && str[1] == ':')
    {
        package = g.keyword();
        symbol_name = str.substr(2);
    }
    else if (str.size() > 1 && str[0] == ':')
    {
        package = g.keyword();
        symbol_name = str.substr(1);
    }
    else
    {
        auto i = str.find(':');
        if (i == std::string::npos) // no colons
        {
            package = g.packages.current();
            symbol_name = str;
        }
        else if (i+1 < str.size() && str[i+1] == ':') // two colons
        {
            package_name = str.substr(0, i);
            package = g.packages.find(package_name);
            symbol_name = str.substr(i+2);
        }
        else // one colon
        {
            package_name = str.substr(0, i);
            package = g.packages.find(package_name);
            symbol_name = str.substr(i+1);
            external = true;
        }
    }

    if (package == nullptr)
    {
        GC_GUARD();
        auto signal_args = gc.list(g.s_SIMPLE_ERROR,
                                   gc.alloc_string("Package does not exist"),
                                   gc.alloc_string(package_name));
        GC_UNGUARD();
        throw VM_State::Signal_Exception(signal_args);
    }

    if (symbol_name.find(':') != std::string::npos)
    {
        GC_GUARD();
        auto signal_args = gc.list(g.s_SIMPLE_ERROR,
                                   gc.alloc_string("Too many colons in symbol name"),
                                   gc.alloc_string(str));
        GC_UNGUARD();
        throw VM_State::Signal_Exception(signal_args);
    }

    if (symbol_name.size() == 0)
    {
        GC_GUARD();
        auto signal_args = gc.list(g.s_SIMPLE_ERROR,
                                   gc.alloc_string("A package was specifed with no symbol"),
                                   gc.alloc_string(str));
        GC_UNGUARD();
        throw VM_State::Signal_Exception(signal_args);
    }

    if (!external)
    {
        return package->intern_symbol(symbol_name);
    }

    Value symbol;
    Package::Symbol_Location_Type location;
    if (package->find_symbol(symbol_name, symbol, &location)
        && location == Package::Symbol_Location_Type::External)
    {
        return symbol;
    }
    GC_GUARD();
    auto signal_args = gc.list(g.s_SIMPLE_ERROR,
                               gc.alloc_string("No external symbol in package"),
                               gc.alloc_string(symbol_name),
                               gc.alloc_string(package_name));
    GC_UNGUARD();
    throw VM_State::Signal_Exception(signal_args);
}

static
bool read(std::istream &source, Value &out_result)
{
    auto c = peek_consuming(source);

    while ((c = peek_consuming(source)) == ';')
    {
        while (!source.eof() && source.peek() != '\n')
        {
            source.get();
        }
    }

    if (source.eof())
    {
        return false;
    }

    if (is_digit(c) || c == '-' || c == '+')
    {
        std::string str;
        str += c;
        source.get();
        int decimal_count = 0;
        while (!source.eof() && (is_digit(source.peek()) || source.peek() == '.'))
        {
            decimal_count += (source.peek() == '.');

            str += source.get();
        }
        bool is_number = source.eof() || !is_symbol_char(source.peek());
        while (!source.eof() && is_symbol_char(source.peek()))
        {
            str += source.get();
        }
        if (is_number && str.size() == 1 && (str[0] == '-' || str[0] == '+'))
        {
            is_number = false;
        }
        if (is_number && decimal_count < 2)
        {
            if (decimal_count != 0)
            {
                out_result = gc.alloc_object<Float>(std::stod(str));
                return true;
            }
            else
            {
                out_result = Value::wrap_fixnum(std::stoll(str));
                return true;
            }
        }
        out_result = make_symbol(str_upper(str));
        return true;
    }

    if (c == '#')
    {
        source.get();
        if (source.peek() == '\'')
        {
            source.get();
            Value func;
            if (read(source, func))
            {
                out_result = gc.list(g.s_FUNCTION, func);
                return true;
            }
            return false;
        }
        if (source.peek() == '\\')
        {
            source.get();
            std::string character;
            while (!source.eof() && is_symbol_char(source.peek()))
            {
                character += source.get();
            }
            if (character.size() == 0)
            {
                if (source.eof())
                {
                    return false;
                }
                character += source.get();
            }

            if (character.size() == 1)
            {
                out_result = Value::wrap_character(character[0]);
                return true;
            }

            character = str_upper(character);

            if (character == "SPACE")
            {
                out_result = Value::wrap_character(' ');
            }
            else if (character == "RETURN")
            {
                out_result = Value::wrap_character('\r');
            }
            else if (character == "NEWLINE")
            {
                out_result = Value::wrap_character('\n');
            }
            else if (character == "TAB")
            {
                out_result = Value::wrap_character('\t');
            }
            else
            {
                auto it = *reinterpret_cast<const int32_t*>(character.c_str());
                out_result = Value::wrap_character(it);
            }
            return true;
        }
        return false;
    }

    if (c == '"')
    {
        source.get();
        std::string val;
        while (!source.eof() && source.peek() != '"')
        {
            auto c = source.get();
            if (c == '\\')
            {
                c = source.get();
                switch (c)
                {
                    case 'r': c = '\r'; break;
                    case 't': c = '\t'; break;
                    case 'n': c = '\n'; break;
                }
            }
            val += c;
        }
        source.get();
        out_result = gc.alloc_string(val);
        return true;
    }

    if (c == '\'')
    {
        source.get();
        Value quoted;
        if (read(source, quoted))
        {
            out_result = gc.list(g.s_QUOTE, quoted);
            return true;
        }
        return false;
    }

    if (c == '`')
    {
        source.get();
        Value quoted;
        if (read(source, quoted))
        {
            out_result = gc.list(g.s_QUASIQUOTE, quoted);
            return true;
        }
        return false;
    }

    if (c == ',')
    {
        source.get();
        auto symbol = g.s_UNQUOTE;
        if (source.peek() == '@')
        {
            source.get();
            symbol = g.s_UNQUOTE_SPLICING;
        }
        Value value;
        if (read(source, value))
        {
            out_result = gc.list(symbol, value);
            return true;
        }
        return false;
    }

    if (is_symbol_start_char(c))
    {
        std::string str;
        while (!source.eof() && is_symbol_char(source.peek()))
        {
            str += source.get();
        }
        str = str_upper(str);
        if (str == "NIL")
        {
            out_result = Value::nil();
        }
        else
        {
            out_result = make_symbol(str);
        }
        return true;
    }

    if (c == '(')
    {
        source.get();
        if (peek_consuming(source) == ')')
        {
            source.get();
            out_result = Value::nil();
            return true;
        }

        Value car_obj;
        if (!read(source, car_obj))
        {
            return false;
        }
        auto head = gc.list(car_obj);
        car_obj = head;

        while (!source.eof() && peek_consuming(source) != ')')
        {
            if (source.peek() == '.')
            {
                source.get();
                Value cdr_obj;
                if (!read(source, cdr_obj))
                {
                    return false;
                }
                set_cdr(car_obj, cdr_obj);
                break;
            }

            Value elem;
            if (!read(source, elem))
            {
                return false;
            }
            set_cdr(car_obj, gc.list(elem));
            car_obj = cdr(car_obj);
        }

        if (source.eof() || peek_consuming(source) != ')')
        {
            return false;
        }
        source.get();

        out_result = head;
        return true;
    }

    return false;
}

bool read_gc_paused(std::istream &source, Value &out_result)
{
    GC_GUARD();
    auto result = read(source, out_result);
    GC_UNGUARD();
    return result;
}


void trace_signal_exception(VM_State &vm, const VM_State::Signal_Exception &e)
{
    if (e.stack_trace_top != e.stack_trace_bottom)
    {
        auto p = e.stack_trace_bottom;
        for (; p != e.stack_trace_top; ++p)
        {
            if (p->ip)
            {
                bytecode::disassemble_maybe_function(std::cout, "WHOOPS", p->ip);
            }
        }
    }

    if (e.ip)
    {
        bytecode::disassemble_maybe_function(std::cout, "WHOOPS", e.ip);
    }
    printf("ERROR: %s\n", repr(e.what).c_str());
    //auto signal = car(e.what);
    //auto signal_args = cdr(e.what);
}

bool eval_file(VM_State &vm, compiler::Scope &root_scope, const std::filesystem::path &path)
{
    static auto load_sym = g.core()->export_symbol("LOAD").as_object()->symbol();
    if (!load_sym->function().is_nil())
    {
        auto path_str = gc.alloc_string(path);
        try
        {
            vm.call_lisp_function(load_sym->function(), &path_str, 1);
        }
        catch (VM_State::Signal_Exception e)
        {
            trace_signal_exception(vm, e);
            return false;
        }
        return true;
    }
    else
    {
        std::ifstream stm(path, std::ios::binary);
        if (!stm.is_open())
        {
            fprintf(stderr, "cannot open file: %s\n", path.c_str());
            return false;
        }

        set_global(&root_scope,
                   g.core()->export_symbol("*FILE-PATH*"),
                   gc.alloc_string(path));

        auto here_path = std::filesystem::current_path();
        auto there_path = path.parent_path();
        if (there_path != "")
        {
            std::filesystem::current_path(there_path);
        }

        while (!stm.eof())
        {
            Value out;
            if (read_gc_paused(stm, out))
            {
                auto out_handle = gc.pin_value(out);
                try
                {
                    auto expanded = macro_expand_impl(out, vm);
                    gc.unpin_value(out_handle);

                    bytecode::Emitter e(&root_scope);
                    compiler::compile(e, expanded, true, false);
                    e.emit_halt();
                    e.lock();

                    g.resize_globals(root_scope.locals().size());

                    vm.push_frame(nullptr, 0);
                    vm.execute(e.bytecode().data());
                }
                catch (VM_State::Signal_Exception e)
                {
                    trace_signal_exception(vm, e);
                    return false;
                }
                if (vm.stack_top() != vm.stack_bottom())
                {
                    vm.pop_param();
                }

                if (vm.call_frame_top() != vm.call_frame_bottom())
                {
                    vm.set_frame(vm.pop_frame());
                }
            }
            else if (stm.eof())
            {
                break;
            }
        }
        std::filesystem::current_path(here_path);
        return true;
    }
}

void run_repl(VM_State &vm, compiler::Scope &root_scope)
{
    set_global(&root_scope,
               g.core()->export_symbol("*FILE-PATH*"),
               gc.alloc_string("<stdin>"));


    g.packages.in_package(g.user());
    set_global(&root_scope,
               g.core()->export_symbol("*PACKAGE*"),
               g.packages.current()->as_lisp_value());

    auto &stm = std::cin;
    while (!stm.eof())
    {
        printf("%s> ", g.packages.current()->name().c_str());
        Value out;
        try {
            if (read_gc_paused(stm, out))
            {
                auto out_handle = gc.pin_value(out);
                try
                {
                    auto expanded = macro_expand_impl(out, vm);
                    gc.unpin_value(out_handle);

                    bytecode::Emitter e(&root_scope);
                    compiler::compile(e, expanded, true, false);
                    e.emit_halt();
                    e.lock();

                    g.resize_globals(root_scope.locals().size());

                    vm.push_frame(nullptr, 0);
                    vm.execute(e.bytecode().data());
                }
                catch (VM_State::Signal_Exception e)
                {
                    gc.unpin_value(out_handle);
                    trace_signal_exception(vm, e);
                    continue;
                }
                if (vm.stack_top() != vm.stack_bottom())
                {
                    auto result = vm.pop_param();
                    printf("==> %s\n", repr(result).c_str());
                }

                if (vm.call_frame_top() != vm.call_frame_bottom())
                {
                    vm.set_frame(vm.pop_frame());
                }
            }
            else if (stm.eof())
            {
                break;
            }
        }
        catch (VM_State::Signal_Exception e)
        {
            trace_signal_exception(vm, e);
            continue;
        }
    }
}

}


int main(int argc, char **argv)
{
    using namespace lisp;
    bool use_boot = true;
    bool repl = false;
    char *file = nullptr;

    int i = 1;
    for (; i < argc; ++i)
    {
        if (argv[i][0] == '-')
        {
            if (strcmp("--no-boot", argv[i]) == 0)
            {
                use_boot = false;
            }
            else if (strcmp("--boot", argv[i]) == 0)
            {
                use_boot = true;
            }
            else if (strcmp("-i", argv[i]) == 0)
            {
                repl = true;
            }
            else if (strcmp("-", argv[i]) == 0)
            {
                i++;
                break;
            }
        }
        else
        {
            file = argv[i];
            break;
        }
    }

    repl = repl || !file;

    compiler::THE_ROOT_SCOPE = new compiler::Scope;
    initialize_globals(compiler::THE_ROOT_SCOPE, argv+i);

    g.packages.in_package(g.user());

    auto vm = THE_LISP_VM = new VM_State;

    gc.set_paused(false);
    if (use_boot)
    {
        auto exe_dir = plat::get_executable_path().parent_path();
        auto boot_path = exe_dir/"lib"/"boot.lisp";
        eval_file(*vm, *compiler::THE_ROOT_SCOPE, std::filesystem::absolute(boot_path));
    }

    if (file)
    {
        eval_file(*vm, *compiler::THE_ROOT_SCOPE, file);
    }

    if (repl)
    {
        run_repl(*vm, *compiler::THE_ROOT_SCOPE);
    }

#if PROFILE_OPCODE_PAIRS
    std::vector<std::pair<Opcode_Pair, int>> results;
    for (auto &[k, v] : vm->opcode_pairs())
    {
        results.push_back({k, v});
    }
    std::sort(results.begin(), results.end(), [](const std::pair<Opcode_Pair, int> &a,
                                                 const std::pair<Opcode_Pair, int> &b) {
                                                  return a.second > b.second;
                                              });
    int count = 30;
    for (auto &[pair, times] : results)
    {
        std::cout << bytecode::opcode_name(static_cast<bytecode::Opcode>(pair.a)) << " : "
                  << bytecode::opcode_name(static_cast<bytecode::Opcode>(pair.b)) << " -> "
                  << times << "\n";
        if (count-- == 0)
        {
            break;
        }
    }
#endif

    return 0;
}
