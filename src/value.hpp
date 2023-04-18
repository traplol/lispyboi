#ifndef _LISPYBOI_VALUE_
#define _LISPYBOI_VALUE_

#include <stdint.h>
#include <type_traits>
#include <iostream>
#include "defines.hpp"

template <class...> constexpr std::false_type always_false{};

namespace lisp
{

using Fixnum = int64_t;
using Float = double;

struct Cons;
struct Object;
struct Value;

struct Symbol;
struct Closure;
struct Package;
struct File_Stream;
struct Simple_Array;
struct Structure;
struct Function;

using System_Pointer = void*;
using Primitive = Value (*)(Value *args, uint32_t nargs, bool &raised_signal);

enum class Object_Type
{
    Symbol,
    Closure,
    Package,
    File_Stream,
    Simple_Array,
    System_Pointer,
    Structure,
    Float
};

struct Value
{
    using Bits_Type = uint64_t;

    FORCE_INLINE
    Value() = default;

    constexpr explicit FORCE_INLINE
    Value(Bits_Type bits) noexcept
        : m_bits(bits)
    {}

    FORCE_INLINE
    bool operator==(const Value &other) const noexcept
    {
        return m_bits == other.m_bits;
    }

    FORCE_INLINE
    bool operator!=(const Value &other) const noexcept
    {
        return m_bits != other.m_bits;
    }

    // The operators (+, -, +=, and -=) are implemented in a way that allows us to abuse
    // the underlying representation where LSB(0) is 0.
    FORCE_INLINE
    Value operator+(const Value &other) const noexcept
    {
        return Value(m_bits + other.m_bits);
    }

    FORCE_INLINE
    Value operator-(const Value &other) const noexcept
    {
        return Value(m_bits - other.m_bits);
    }

    FORCE_INLINE
    Value &operator+=(const Value &other) noexcept
    {
        m_bits += other.m_bits;
        return *this;
    }

    FORCE_INLINE
    Value &operator-=(const Value &other) noexcept
    {
        m_bits -= other.m_bits;
        return *this;
    }

    FORCE_INLINE
    Value operator|(const Value &other) const noexcept
    {
        return Value(m_bits | other.m_bits);
    }

    FORCE_INLINE
    Value &operator|=(const Value &other) noexcept
    {
        m_bits |= other.m_bits;
        return *this;
    }

    FORCE_INLINE
    Bits_Type bits() const noexcept
    {
        return m_bits;
    }

    FORCE_INLINE
    Bits_Type tag_bits() const noexcept
    {
        return m_bits & 0b111;
    }

    FORCE_INLINE
    Bits_Type wide_tag_bits() const noexcept
    {
        return m_bits & 0xFF;
    }

    static FORCE_INLINE
    Value wrap_fixnum(Fixnum fixnum) noexcept
    {
        union
        {
            struct
            {
                Fixnum tag : 1;
                Fixnum value : 63;
            } f;
            Bits_Type bits;
        } u;
        u.f.tag = 0;
        u.f.value = fixnum;
        return Value(u.bits);
    }

    static FORCE_INLINE
    Value wrap_cons(Cons *cons) noexcept
    {
        union
        {
            Cons *cons;
            Bits_Type bits;
        } u;
        u.cons = cons;
        u.bits |= TAG_CONS;
        return Value(u.bits);
    }

    static FORCE_INLINE
    Value wrap_object(Object *object) noexcept
    {
        union
        {
            Object *object;
            Bits_Type bits;
        } u;
        u.object = object;
        u.bits |= TAG_OBJECT;
        return Value(u.bits);
    }

    static FORCE_INLINE
    Value wrap_primitive(Primitive func) noexcept
    {
        union
        {
            Primitive func;
            Bits_Type bits;
        } u;
        u.func = func;
        u.bits <<= 3;
        u.bits |= TAG_PRIM_FUNC;
        return Value(u.bits);
    }

    static FORCE_INLINE
    Value wrap_character(int32_t codepoint) noexcept
    {
        union
        {
            struct
            {
                int32_t _unused;
                int32_t codepoint;
            } c;
            Bits_Type bits;
        } u;
        u.bits = WTAG_CHAR;
        u.c.codepoint = codepoint;
        return Value(u.bits);
    }

    static FORCE_INLINE
    Value nil() noexcept
    {
        return Value(WTAG_NIL);
    }

    FORCE_INLINE
    bool is_fixnum() const noexcept
    {
        return (bits() & 1) == 0;
    }

    FORCE_INLINE
    bool is_character() const noexcept
    {
        return wide_tag_bits() == WTAG_CHAR;
    }

    FORCE_INLINE
    bool is_nil() const noexcept
    {
        return nil() == *this;
    }

    FORCE_INLINE
    bool is_cons() const noexcept
    {
        return tag_bits() == TAG_CONS;
    }

    FORCE_INLINE
    bool is_list() const noexcept
    {
        return is_nil() || is_cons();
    }

    FORCE_INLINE
    bool is_garbage_collected() const noexcept
    {
        return is_cons() || is_object();
    }

    FORCE_INLINE
    bool is_lisp_primitive() const noexcept
    {
        return tag_bits() == TAG_PRIM_FUNC;
    }

    FORCE_INLINE
    bool is_object() const noexcept
    {
        return tag_bits() == TAG_OBJECT;
    }

    bool is_type(Object_Type type) const noexcept;

    FORCE_INLINE
    Fixnum as_fixnum() const noexcept
    {
        ENSURE_VALUE(this, is_fixnum());
        union
        {
            struct
            {
                Fixnum tag : 1;
                Fixnum value : 63;
            } f;
            Bits_Type bits;
        } u;
        u.bits = m_bits;
        return u.f.value;
    }

    FORCE_INLINE
    Cons *as_cons() const noexcept
    {
        ENSURE_VALUE(this, is_cons());
        union
        {
            Cons *cons;
            Bits_Type bits;
        } u;
        u.bits = m_bits & ~BITS_MASK;
        return u.cons;
    }

    FORCE_INLINE
    Object *as_object() const noexcept
    {
        ENSURE_VALUE(this, is_object());
        union
        {
            Object *object;
            Bits_Type bits;
        } u;
        u.bits = m_bits & ~BITS_MASK;
        return u.object;
    }

    FORCE_INLINE
    Primitive as_lisp_primitive() const noexcept
    {
        ENSURE_VALUE(this, is_lisp_primitive());
        union
        {
            Primitive func;
            Bits_Type bits;
        } u;
        u.bits = m_bits;
        u.bits >>= 3;
        return u.func;
    }

    FORCE_INLINE
    int32_t as_character() const noexcept
    {
        ENSURE_VALUE(this, is_character());
        union
        {
            struct
            {
                int32_t _unused;
                int32_t codepoint;
            } c;
            Bits_Type bits;
        } u;
        u.bits = m_bits;
        return u.c.codepoint;
    }

    FORCE_INLINE
    void *unwrap_pointer() const noexcept
    {
        return reinterpret_cast<void*>(bits() & ~BITS_MASK);
    }

  private:
    // Do not be fooled into thinking this is another layer of indirection.
    // This struct, or rather _value_, is a mere 8-bytes and allows us to
    // represent several primitive data-types alongside a more general object
    // structure. On 64-bit arch a pointer is 8 bytes so word-aligned
    // addresses have the LSB(3) bits set to 0. We can exploit this and give
    // meaning to those bits.

    // bits 7 6 5 4 3 2 1 0
    // --------------------
    // - - - - - - - 0 -> FIXNUM
    // t t t t t 0 1 1 -> Other immediate; the 5 't' bits are extra tag bits
    //                    this gives us 2^5 = 32 distinct tags and 56 bits
    //                    worth of storage. We can store things like:
    //                    byte literal, utf-8 codepoints, 32-bit floats,
    //                    56-bit bit vector, etc.
    // - - - - - 1 0 1 -> Cons
    // - - - - - 1 1 1 -> Primitive Function
    // - - - - - 0 0 1 -> POINTER/OBJECT

    // You'll notice we don't reuse LSB(0) and that is because we want to
    // maximize the range of fixnums. This means when there is a 0 at LSB(0)
    // then the other 63 bits must represent the fixnum.

    // Another side-effect of this decision is that fixnum adds and subracts
    // do not need to be unwrapped because a LSB(0) of 0 means the number
    // is even and adding two even integers always results in an even integer,
    // effectively meaning LSB(0) has no impact to the underlying fixnum.
    static constexpr Bits_Type BITS_MASK = 0b111ULL;

    static constexpr Bits_Type TAG_OBJECT    = 0b001ULL;
    static constexpr Bits_Type TAG_OTHER_IMM = 0b011ULL;
    static constexpr Bits_Type TAG_CONS      = 0b101ULL;
    static constexpr Bits_Type TAG_PRIM_FUNC = 0b111ULL;

    static constexpr Bits_Type WTAG_NIL      = (0b00000ULL << 3) | TAG_OTHER_IMM;
    static constexpr Bits_Type WTAG_CHAR     = (0b00001ULL << 3) | TAG_OTHER_IMM;

    Bits_Type m_bits;
};

static_assert(std::is_trivial<Value>::value, "Value not trivial type.");

struct Closure_Reference
{
    Closure_Reference()
        : next(nullptr)
        , m_location(nullptr)
        , m_closed_value(0)
    {}

    Closure_Reference(Value *location)
        : next(nullptr)
        , m_location(location)
        , m_closed_value(0)
    {}

    FORCE_INLINE
    void close()
    {
        m_closed_value = *m_location;
        m_location = &m_closed_value;
    }

    Value &value()
    {
        return *m_location;
    }

    const Value *location() const
    {
        return m_location;
    }

    Closure_Reference *next;
  private:
    Value *m_location;
    Value m_closed_value;
};

bool read_gc_paused(std::istream &source, Value &out_result);
std::string repr(Value value);

namespace bytecode
{
int disassemble(std::ostream &out, const std::string &tag, const Function *function, const uint8_t *ip = nullptr);
}

}


#endif
