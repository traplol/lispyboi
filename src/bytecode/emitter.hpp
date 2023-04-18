#ifndef _LISPYBOI_BYTECODE_EMITTER_
#define _LISPYBOI_BYTECODE_EMITTER_

#include <list>
#include <vector>
#include <unordered_map>
#include <assert.h>

namespace lisp
{

namespace compiler
{
struct Scope;
}

namespace bytecode
{

struct Emitter
{
    Emitter(compiler::Scope *scope)
        : m_scope(scope)
        , m_locked(false)
    {}

    void emit_push_literal(Value value);
    void emit_push_nil();
    void emit_push_fixnum_0();
    void emit_push_fixnum_1();

    void emit_funcall(uint32_t argc);
    void emit_gotocall(uint32_t argc);
    void emit_apply(uint32_t argc);

    void emit_return();

    int32_t emit_jump();
    int32_t emit_pop_jump_if_nil();

    void emit_get_value(Value value);
    void emit_set_value(Value value);

    void emit_function_value(Value func_val);

    void emit_instantiate_closure(const Function *function);
    void emit_close_values(uint32_t num_values);

    void emit_pop();

    void emit_halt();

    int32_t emit_push_handler_case(uint32_t num_handlers);
    void emit_pop_handler_case();

    void emit_raise_signal(uint32_t argc);

    void emit_cons();
    void emit_car();
    void emit_cdr();
    void emit_eq();
    void emit_rplaca();
    void emit_rplacd();
    void emit_aref();
    void emit_aset();

    void emit_add();
    void emit_add_1();
    void emit_sub();
    void emit_sub_1();

    void emit_debug_trap();

    template<typename T>
    void set_raw(size_t offset, T value)
    {
        assert(offset + sizeof(value) <= m_bytecode.size());
        T *slot = reinterpret_cast<T*>(m_bytecode.data() + offset);
        *slot = value;
    }

    void lock();
    void map_range_to(size_t begin, size_t end, Value expr);
    int32_t position() const;
    const std::vector<uint8_t> &bytecode() const;
    std::vector<uint8_t> &&move_bytecode();

    void push_labels();
    void pop_labels();
    bool get_label(Value tag, int32_t &out_offset);
    int32_t make_label(Value tag);
    void backfill_label(int32_t offset, Value tag);

    compiler::Scope *scope() const;

  private:
    void emit_get_value(Symbol *symbol);
    void emit_set_value(Symbol *symbol);
    struct Backfill_Info
    {
        Value tag;
        int32_t offset;
    };

    using Label_Map = std::unordered_map<Value, int32_t>;

    template<typename T>
    void append(T value)
    {
        if (m_locked)
        {
            fprintf(stderr, "cannot write to locked bytecode emitter!\n");
            abort();
        }
        m_bytecode.resize(m_bytecode.size() + sizeof(T), 0xCC);
        auto end = m_bytecode.data() + m_bytecode.size();
        T *slot = reinterpret_cast<T*>(end - sizeof(T));
        *slot = value;
    }

    struct Range_Value_Pair
    {
        size_t begin;
        size_t end;
        Value value;
    };

    std::vector<Range_Value_Pair> m_debug_map;

    std::vector<uint8_t> m_bytecode;
    std::vector<Label_Map> m_label_stack;
    std::list<Backfill_Info> m_backfills;
    compiler::Scope *m_scope;

    bool m_locked;
};

}
}

#endif
