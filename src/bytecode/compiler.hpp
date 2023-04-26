#ifndef _LISPYBOI_BYTECODE_COMPILER_
#define _LISPYBOI_BYTECODE_COMPILER_

#include <vector>
#include "../value.hpp"

namespace lisp
{
namespace bytecode
{
class Emitter;
}
namespace compiler
{


struct Scope;

struct Variable
{
    const Symbol *symbol() const
    {
        return m_symbol;
    }

    bool is_captured() const
    {
        return m_is_captured;
    }

    bool is_global() const;

    Scope *scope()
    {
        return m_scope;
    }

  private:
    friend struct Scope;
    Variable(const Symbol *symbol, Scope *scope)
        : m_symbol(symbol)
        , m_scope(scope)
    {}
    const Symbol *m_symbol;
    Scope *m_scope;
    bool m_is_captured;
};

struct Scope
{
    struct Capture_Info
    {
        Symbol *symbol;
        uint32_t index;
        bool is_local;
    };

    Scope()
        : m_parent(nullptr)
    {}

    Scope(Scope *parent)
        : m_parent(parent)
        , m_stack_space_needed(0)
    {}

    bool is_root() const
    {
        return m_parent == nullptr;
    }

    Scope *get_root()
    {
        auto p = this;
        while (p->m_parent)
        {
            p = p->m_parent;
        }
        return p;
    }

    Scope *parent()
    {
        return m_parent;
    }

    Scope *push_scope()
    {
        return new Scope(this);
    }

    const std::vector<Capture_Info> &capture_info() const
    {
        return m_captures;
    }

    const std::vector<Variable*> &locals() const
    {
        return m_locals;
    }

    const size_t stack_space_needed() const
    {
        return m_stack_space_needed;
    }

    Variable *create_variable(const Symbol *symbol, uint32_t *opt_out_idx = nullptr)
    {
        auto var = new Variable(symbol, this);
        if (opt_out_idx != nullptr)
        {
            *opt_out_idx = m_locals.size();
        }
        m_locals.push_back(var);
        m_stack_space_needed = std::max(m_stack_space_needed, m_locals.size());
        return var;
    }

    void pop_variables(size_t n)
    {
        m_locals.resize(m_locals.size() - n);
    }

    bool resolve_local(Symbol *symbol, uint32_t &out_idx)
    {
        if (m_locals.size() != 0)
        {
            for (uint32_t i = m_locals.size(); i > 0; --i)
            {
                auto local = m_locals[i - 1];
                if (local->symbol() == symbol)
                {
                    out_idx = i - 1;
                    return true;
                }
            }
        }
        return false;
    }

    bool resolve_capture(Symbol *symbol, uint32_t &out_idx)
    {
        if (m_parent == nullptr || m_parent->is_root())
        {
            return false;
        }

        uint32_t idx;
        if (m_parent->resolve_local(symbol, idx))
        {
            return capture(symbol, idx, true, out_idx);
        }

        if (m_parent->resolve_capture(symbol, idx))
        {
            return capture(symbol, idx, false, out_idx);
        }

        return false;
    }


  private:
    friend struct Variable;

    bool capture(Symbol *symbol, uint32_t index, bool is_local, uint32_t &out_idx)
    {
        if (is_root())
        {
            return false;
        }

        for (uint32_t i = 0; i < m_captures.size(); ++i)
        {
            if (m_captures[i].symbol == symbol)
            {
                out_idx = i;
                return true;
            }
        }
        out_idx = m_captures.size();
        m_captures.push_back({symbol, index, is_local});
        return true;
    }

    Scope *m_parent;
    std::vector<Variable*> m_locals;
    std::vector<Capture_Info> m_captures;
    size_t m_stack_space_needed;
};

extern Scope *THE_ROOT_SCOPE;

void compile(bytecode::Emitter &e, Scope *scope, Value expr, bool toplevel, bool tail_position = false);

}

namespace bytecode
{
struct Debug_Info
{
    struct Region
    {
        Region() = default;
        Region(const void *start, size_t size)
            : m_start(start)
            , m_size(size)
        {}

        bool contains(const void *ptr) const
        {
            auto begin = reinterpret_cast<uintptr_t>(m_start);
            auto end = begin + m_size;
            auto p = reinterpret_cast<uintptr_t>(ptr);

            return begin <= p && p < end;
        }

        const void *begin() const
        {
            return m_start;
        }

        const void *end() const
        {
            return reinterpret_cast<void*>(reinterpret_cast<uintptr_t>(m_start) + m_size);
        }

        size_t size() const
        {
            return m_size;
        }

      private:
        const void *m_start;
        size_t m_size;
    };

    static bool find(const void *address, Value &out_expr);
    static bool find_function(const void *address, const Function **out_function);
    static void push(const void *start, size_t size, Value expr);
    static void track_function(const Function *function);

  private:
    static std::vector<std::pair<Region, Value>> m_bytecode_address_expr_map;
    static std::vector<const Function*> m_functions;
};
}
}

#endif
