#ifndef _LISPYBOI_UTIL_
#define _LISPYBOI_UTIL_

#include <string>

#include "defines.hpp"
#include "value.hpp"
#include "gc.hpp"

namespace lisp
{

static
Fixnum length(Value v)
{
    Fixnum len = 0;
    while (!v.is_nil())
    {
        ++len;
        v = cdr(v);
    }
    return len;
}

static
std::string lisp_string_to_native_string(Value str)
{
    std::string ret;
    auto array = str.as_object()->simple_array();
    for (Fixnum i = 0; i < array->size(); ++i) {
        auto obj = array->at(i);
        if (!obj.is_character()) break;
        auto c = obj.as_character();
        if ((c & 0xf8) == 0xf0) {
            ret.push_back(static_cast<char>((c >>  0) & 0xff));
            ret.push_back(static_cast<char>((c >>  8) & 0xff));
            ret.push_back(static_cast<char>((c >> 16) & 0xff));
            ret.push_back(static_cast<char>((c >> 24) & 0xff));
        }
        else if ((c & 0xf0) == 0xe0) {
            ret.push_back(static_cast<char>((c >>  0) & 0xff));
            ret.push_back(static_cast<char>((c >>  8) & 0xff));
            ret.push_back(static_cast<char>((c >> 16) & 0xff));
        }
        else if ((c & 0xe0) == 0xc0) {
            ret.push_back(static_cast<char>((c >>  0) & 0xff));
            ret.push_back(static_cast<char>((c >>  8) & 0xff));
        }
        else {
            ret.push_back(static_cast<char>(c & 0xff));
        }
    }
    return ret;
}


FORCE_INLINE
bool Value::is_type(Object_Type type) const noexcept
{
    return is_object() && as_object()->type() == type;
}

static FORCE_INLINE
bool symbolp(Value v)
{
    return v.is_type(Object_Type::Symbol);
}

static //FORCE_INLINE
Value to_list(const Value *vals, uint32_t nvals)
{
    switch (nvals)
    {
        case 0: return Value::nil();
        case 1: return gc.list(vals[0]);
        case 2: return gc.list(vals[0], vals[1]);
        case 3: return gc.list(vals[0], vals[1], vals[2]);
        case 4: return gc.list(vals[0], vals[1], vals[2], vals[3]);
        case 5: return gc.list(vals[0], vals[1], vals[2], vals[3], vals[4]);
        case 6: return gc.list(vals[0], vals[1], vals[2], vals[3], vals[4], vals[5]);
        case 7: return gc.list(vals[0], vals[1], vals[2], vals[3], vals[4], vals[5], vals[6]);
        case 8: return gc.list(vals[0], vals[1], vals[2], vals[3], vals[4], vals[5], vals[6], vals[7]);
    }

    std::vector<GC::Ptr_Handle> val_handles(nvals);
    for (uint32_t i = 0; i < nvals; ++i)
    {
        val_handles.push_back(gc.pin_value(vals[i]));
    }

    auto head = gc.list(vals[0]);
    auto head_handle = gc.pin_value(head);
    gc.unpin_value(val_handles[0]);
    auto current = head;

    for (uint32_t i = 1; i < nvals; ++i)
    {
        set_cdr(current, gc.cons(vals[i], Value::nil()));
        gc.unpin_value(val_handles[i]);
        current = cdr(current);
    }

    gc.unpin_value(head_handle);
    return head;
}

static //FORCE_INLINE
Value to_list(const std::vector<Value> &vals)
{
    return to_list(vals.data(), vals.size());
}

static //FORCE_INLINE
std::vector<Value> to_vector(Value list)
{
    std::vector<Value> v;
    while (!list.is_nil())
    {
        v.push_back(car(list));
        list = cdr(list);
    }
    return v;
}

}


namespace lisp
{

struct Opcode_Pair
{
    int a;
    int b;

    bool operator==(const Opcode_Pair &other) const noexcept
    {
        return a == other.a && b == other.b;
    }

    bool operator!=(const Opcode_Pair &other) const noexcept
    {
        return !(*this == other);
    }
};

}

namespace std
{

template <>
struct hash<lisp::Opcode_Pair>
{
    std::size_t operator()(const lisp::Opcode_Pair& p) const
    {
        return p.a << 8 | p.b;
    }
};

}

namespace std
{

template <>
struct hash<lisp::Value>
{
    std::size_t operator()(const lisp::Value& v) const
    {
        return std::hash<lisp::Value::Bits_Type>()(v.bits());
    }
};

}

#endif
