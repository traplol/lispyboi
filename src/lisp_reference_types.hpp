#ifndef _LISPYBOI_LISP_REFERENCE_TYPES_
#define _LISPYBOI_LISP_REFERENCE_TYPES_

#include <string>
#include <fstream>
#include <vector>
#include <string.h>

#include "defines.hpp"
#include "value.hpp"

namespace lisp
{
struct Cons
{
    Value car;
    Value cdr;
};

FORCE_INLINE
void set_car(Value cons, Value val)
{
    cons.as_cons()->car = val;
}

FORCE_INLINE
void set_cdr(Value cons, Value val)
{
    cons.as_cons()->cdr = val;
}

FORCE_INLINE
Value car(Value obj)
{
    if (obj.is_nil()) return obj;
    return obj.as_cons()->car;
}

FORCE_INLINE
Value cdr(Value obj)
{
    if (obj.is_nil()) return obj;
    return obj.as_cons()->cdr;
}

FLATTEN inline Value cddr(Value obj)   { return cdr(cdr(obj)); }
FLATTEN inline Value cdddr(Value obj)  { return cdr(cdr(cdr(obj))); }
FLATTEN inline Value cadr(Value obj)   { return car(cdr(obj)); }
FLATTEN inline Value caddr(Value obj)  { return car(cdr(cdr(obj))); }
FLATTEN inline Value cadddr(Value obj) { return car(cdr(cdr(cdr(obj)))); }
FLATTEN inline Value caar(Value obj)   { return car(car(obj)); }
FLATTEN inline Value cdar(Value obj)   { return cdr(car(obj)); }
FLATTEN inline Value first(Value obj)  { return car(obj); }
FLATTEN inline Value rest(Value obj)   { return cdr(obj); }
FLATTEN inline Value second(Value obj) { return cadr(obj); }
FLATTEN inline Value third(Value obj)  { return caddr(obj); }
FLATTEN inline Value fourth(Value obj) { return cadddr(obj); }

struct Symbol
{
    Symbol(const std::string &name)
        : m_name(name)
        , m_function(Value::nil())
        , m_package(nullptr)
    {}

    Symbol(const std::string &name, Package *package)
        : m_name(name)
        , m_function(Value::nil())
        , m_package(package)
    {}

    Symbol(const std::string &name, Value function, Package *package)
        : m_name(name)
        , m_function(function)
        , m_package(package)
    {}

    bool is_keyword() const;
    std::string qualified_name() const;
    const std::string &name() const
    {
        return m_name;
    }

    bool has_function() const
    {
        return m_function != Value::nil();
    }

    Value function() const
    {
        return m_function;
    }

    void function(Value new_func)
    {
        m_function = new_func;
    }

    Package *package() const
    {
        return m_package;
    }
  private:
    friend struct GC;
    std::string m_name;
    Value m_function;
    Package *m_package;
};

struct Structure
{
    Structure(Value type, Fixnum num_slots)
        : m_type(type)
        , m_num_slots(num_slots)
        , m_slots(new Value[num_slots])
    {}

    ~Structure()
    {
        delete[] m_slots;
    }

    Value type() const
    {
        return m_type;
    }

    Fixnum num_slots() const
    {
        return m_num_slots;
    }

    Value &slot_value(Fixnum slot)
    {
        return m_slots[slot];
    }

    Value type_name() const
    {
        return car(m_type);
    }

  private:
    friend struct GC;
    Value m_type;
    Fixnum m_num_slots;
    Value *m_slots;
};

struct Simple_Array
{
    Simple_Array(Value element_type, Fixnum capacity)
        : m_element_type(element_type)
        , m_capacity(capacity)
        , m_fill_pointer(capacity)
        , m_buffer(capacity <= 0 ? nullptr : new Value[capacity]{Value(0)})
    {}

    Simple_Array(Value element_type, Fixnum capacity, Fixnum fill_pointer)
        : m_element_type(element_type)
        , m_capacity(capacity)
        , m_fill_pointer(fill_pointer)
        , m_buffer(capacity <= 0 ? nullptr : new Value[capacity]{Value(0)})
    {}

    ~Simple_Array()
    {
        delete[] m_buffer;
    }

    Value element_type() const
    {
        return m_element_type;
    }

    Fixnum size() const
    {
        return m_fill_pointer;
    }

    Fixnum capacity() const
    {
        return m_capacity;
    }

    Value &at(Fixnum n) const
    {
        return m_buffer[n];
    }

    void push_back(Value value)
    {
        if (m_capacity == 0)
        {
            m_capacity = 8;
            m_buffer = new Value[m_capacity]{Value(0)};
        }
        else if (m_fill_pointer == m_capacity)
        {
            Fixnum new_capacity = m_fill_pointer * 1.5;
            auto new_buffer = new Value[new_capacity];
            memcpy(new_buffer, m_buffer, sizeof(Value) * m_fill_pointer);
            delete[] m_buffer;
            m_buffer = new_buffer;
            m_capacity = new_capacity;
        }
        m_buffer[m_fill_pointer++] = value;
    }

  private:
    friend struct GC;
    Value m_element_type;
    Fixnum m_capacity;
    Fixnum m_fill_pointer;
    Value *m_buffer;
};

struct File_Stream
{
    File_Stream(const std::string &path, std::ios_base::openmode mode)
        : m_stream(path, mode)
        , m_path(path)
        , m_mode(mode)
    {}

    ~File_Stream()
    {
        m_stream.flush();
    }

    std::fstream &stream()
    {
        return m_stream;
    }

    const std::string &path() const
    {
        return m_path;
    }

    std::ios_base::openmode mode() const
    {
        return m_mode;
    }

    int32_t peek_character()
    {
        if (m_ungetted.size() != 0)
        {
            return m_ungetted.back();
        }
        else
        {
            auto c = read_character();
            m_ungetted.push_back(c);
            return c;
        }
    }

    int32_t read_character()
    {
        if (m_ungetted.size() != 0)
        {
            auto c = m_ungetted.back();
            m_ungetted.pop_back();
            return c;
        }
        else {
            int32_t c = m_stream.get() & 0xff;
            if ((c & 0xf8) == 0)
            {
                c |= (m_stream.get() & 0xff) << 8;
                c |= (m_stream.get() & 0xff) << 16;
                c |= (m_stream.get() & 0xff) << 24;
            }
            else if ((c & 0xf0) == 0xe0)
            {
                c |= (m_stream.get() & 0xff) << 8;
                c |= (m_stream.get() & 0xff) << 16;
            }
            else if ((c & 0xe0) == 0xc0)
            {
                c |= (m_stream.get() & 0xff) << 8;
            }

            return c;
        }
    }

    Fixnum write_byte(char c)
    {
        m_stream.write(&c, 1);
        return 1;
    }

    Fixnum write_character(int32_t c)
    {
        if (m_stream.good())
        {
            auto p = reinterpret_cast<const char*>(&c);
            if ((c & 0xf8) == 0xf0)
            {
                m_stream.write(p, 4);
                return 4;
            }
            else if ((c & 0xf0) == 0xe0)
            {
                m_stream.write(p, 3);
                return 3;
            }
            else if ((c & 0xe0) == 0xc0)
            {
                m_stream.write(p, 2);
                return 2;
            }
            else
            {
                m_stream.write(p, 1);
                return 1;
            }
        }
        return 0;
    }

    void flush()
    {
        m_stream.flush();
    }

    auto tellg()
    {
        return m_stream.tellg();
    }

    void seekg(Fixnum offset, std::ios_base::seekdir dir)
    {
        m_stream.seekg(offset, dir);
    }

  private:
    std::fstream m_stream;
    std::vector<int32_t> m_ungetted;
    std::string m_path;
    std::ios_base::openmode m_mode;
};

struct Function
{
    struct Capture_Offset
    {
        uint32_t index;
        bool is_local;
        std::string name;
    };

    Function(std::vector<uint8_t> &&code,
             std::vector<uint32_t> &&entrypoints,
             std::vector<const Symbol*> &&parameters,
             std::vector<Capture_Offset> &&capture_offsets,
             uint32_t main_entry,
             uint32_t optionals_start_at,
             uint32_t num_locals,
             bool has_rest,
             bool has_optionals)
        : m_code(std::move(code))
        , m_entrypoints(std::move(entrypoints))
        , m_parameters(std::move(parameters))
        , m_capture_offsets(std::move(capture_offsets))
        , m_main_entry(main_entry)
        , m_optionals_start_at(optionals_start_at)
        , m_num_locals(num_locals)
        , m_has_rest(has_rest)
        , m_has_optionals(has_optionals)
    {}

    size_t arity() const
    {
        return m_parameters.size();
    }

    const uint8_t *main_entry() const
    {
        return &m_code[m_main_entry];
    }

    const uint8_t *entrypoint(uint32_t nth_param) const
    {
        if (nth_param >= arity())
        {
            return &m_code[m_main_entry];
        }
        auto ep = m_entrypoints[nth_param];
        return &m_code[ep];
    }

    const uint8_t *begin() const
    {
        return m_code.data();
    }

    const uint8_t *end() const
    {
        return m_code.data() + m_code.size();
    }

    size_t size() const
    {
        return m_code.size();
    }

    const std::vector<const Symbol*> &parameters() const
    {
        return m_parameters;
    }

    const std::vector<Capture_Offset> &capture_offsets() const
    {
        return m_capture_offsets;
    }

    bool has_rest() const
    {
        return m_has_rest;
    }

    uint32_t rest_index() const
    {
        return m_parameters.size() - 1;
    }

    bool has_captures() const
    {
        return m_capture_offsets.size() != 0;
    }

    uint32_t optionals_start_at() const
    {
        return m_optionals_start_at;
    }

    bool has_optionals() const
    {
        return m_has_optionals;
    }

    bool is_too_many_args(uint32_t n) const
    {
        return !has_rest() && n > arity();
    }

    bool is_too_few_args(uint32_t n) const
    {
        if (has_optionals())
        {
            return n < optionals_start_at();
        }
        if (has_rest())
        {
            return n < rest_index();
        }
        return n < arity();
    }

    uint32_t num_locals() const
    {
        return m_num_locals;
    }

    // for gdb
    __attribute__((used, noinline))
    void ez_disassemble() const
    {
        bytecode::disassemble(std::cout, "Function", this, main_entry());
    }

  private:
    std::vector<uint8_t> m_code;
    std::vector<uint32_t> m_entrypoints;
    std::vector<const Symbol*> m_parameters;
    std::vector<Capture_Offset> m_capture_offsets;
    uint32_t m_main_entry;
    uint32_t m_optionals_start_at;
    uint32_t m_num_locals;
    bool m_has_rest;
    bool m_has_optionals;
};

struct Closure
{
    Closure(const Function *function)
        : m_function(function)
    {
        m_captures.resize(function->capture_offsets().size(), nullptr);
    }

    void capture_reference(size_t idx, Closure_Reference *p)
    {
        m_captures[idx] = p;
    }

    Value get_capture(size_t idx) const
    {
        return m_captures[idx]->value();
    }

    void set_capture(size_t idx, Value new_val)
    {
        m_captures[idx]->value() = new_val;
    }

    Closure_Reference *get_reference(size_t idx)
    {
        if (idx >= m_captures.size())
        {
            return nullptr;
        }
        return m_captures[idx];
    }

    const Function *function() const
    {
        return m_function;
    }

    const std::vector<Closure_Reference*> &captures() const
    {
        return m_captures;
    }

  private:
    std::vector<Closure_Reference*> m_captures;
    const Function *m_function;
};

struct Signal_Context
{
    Signal_Context(Value tag, const uint8_t *ip, Value args, std::vector<const uint8_t*>call_stack_trace)
        : m_tag(tag)
        , m_ip(ip)
        , m_args(args)
        , m_call_stack_trace(std::move(call_stack_trace))
    {

    }

    Signal_Context(Value tag, const uint8_t *ip, Value args)
        : m_tag(tag)
        , m_ip(ip)
        , m_args(args)
    {

    }

    ~Signal_Context()
    {
    }

    Value tag() const
    {
        return m_tag;
    }

    const uint8_t *ip() const
    {
        return m_ip;
    }

    Value args() const
    {
        return m_args;
    }

    const std::vector<const uint8_t*> call_stack_trace() const
    {
        return m_call_stack_trace;
    }

  private:
    Value m_tag;
    const uint8_t *m_ip;
    Value m_args;
    std::vector<const uint8_t*> m_call_stack_trace;
};

struct Object
{
    Object_Type type() const
    {
        return m_type;
    }

    Symbol *symbol()
    {
        return as<Symbol>();
    }

    Closure *closure()
    {
        return as<Closure>();
    }

    Package *package()
    {
        return as<Package>();
    }

    File_Stream *file_stream()
    {
        return as<File_Stream>();
    }

    Simple_Array *simple_array()
    {
        return as<Simple_Array>();
    }

    Structure *structure()
    {
        return as<Structure>();
    }

    Float to_float()
    {
        return *as<Float>();
    }

    System_Pointer system_pointer()
    {
        return *as<System_Pointer>();
    }

    void system_pointer(System_Pointer new_val)
    {
        *as<System_Pointer>() = new_val;
    }

    System_Pointer pointer_ref()
    {
        return as<System_Pointer>();
    }

    Signal_Context *signal_context()
    {
        return as<Signal_Context>();
    }

  private:
    friend struct GC;

    template<typename T>
    FORCE_INLINE
    T *as()
    {
        return reinterpret_cast<T*>(&m_data[0]);
    }

    template<typename T, typename... Args>
    FORCE_INLINE
    void construct_data(Args... args)
    {
        if constexpr (std::is_same<T, Symbol>::value)
        {
            m_type = Object_Type::Symbol;
        }
        else if constexpr (std::is_same<T, Closure>::value)
        {
            m_type = Object_Type::Closure;
        }
        else if constexpr (std::is_same<T, Package>::value)
        {
            m_type = Object_Type::Package;
        }
        else if constexpr (std::is_same<T, File_Stream>::value)
        {
            m_type = Object_Type::File_Stream;
        }
        else if constexpr (std::is_same<T, Simple_Array>::value)
        {
            m_type = Object_Type::Simple_Array;
        }
        else if constexpr (std::is_same<T, System_Pointer>::value)
        {
            m_type = Object_Type::System_Pointer;
        }
        else if constexpr (std::is_same<T, Structure>::value)
        {
            m_type = Object_Type::Structure;
        }
        else if constexpr (std::is_same<T, Float>::value)
        {
            m_type = Object_Type::Float;
        }
        else if constexpr (std::is_same<T, Signal_Context>::value)
        {
            m_type = Object_Type::Signal_Context;
        }
        else
        {
            static_assert(always_false<T>);
        }

        new (&m_data[0]) T{args...};
    }

    void destruct_data()
    {
        switch (m_type)
        {
            case Object_Type::Symbol: symbol()->~Symbol(); break;
            case Object_Type::Closure: closure()->~Closure(); break;
            case Object_Type::Package: break;
            case Object_Type::File_Stream: file_stream()->~File_Stream(); break;
            case Object_Type::Simple_Array: simple_array()->~Simple_Array(); break;
            case Object_Type::System_Pointer: break;
            case Object_Type::Structure: structure()->~Structure(); break;
            case Object_Type::Float: break;
            case Object_Type::Signal_Context: signal_context()->~Signal_Context(); break;
        }
    }

    Object_Type m_type;
    char m_data[0];
};

FORCE_INLINE
bool Value::is_type(Object_Type type) const noexcept
{
    return is_object() && as_object()->type() == type;
}

}

#endif
