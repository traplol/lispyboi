#ifndef _LISPYBOI_BYTECODE_EMITTER_
#define _LISPYBOI_BYTECODE_EMITTER_

#include <list>
#include <vector>
#include <unordered_map>
#include <assert.h>

#include "emitter.hpp"

namespace lisp
{

namespace bytecode
{

class BC_Emitter : public Emitter
{
  public:
    BC_Emitter()
        : m_locked(false)
    {}

    virtual Emitter *of_same_type() const override;

    virtual void emit_push_value(Value value) override;
    virtual void emit_push_nil() override;
    virtual void emit_push_fixnum_0() override;
    virtual void emit_push_fixnum_1() override;

    virtual void emit_funcall(uint32_t argc) override;
    virtual void emit_gotocall(uint32_t argc) override;
    virtual void emit_apply(uint32_t argc) override;

    virtual void emit_return() override;

    virtual void emit_get_global(uint32_t idx) override;
    virtual void emit_get_local(uint32_t idx) override;
    virtual void emit_get_capture(uint32_t idx) override;

    virtual void emit_set_global(uint32_t idx) override;
    virtual void emit_set_local(uint32_t idx) override;
    virtual void emit_set_capture(uint32_t idx) override;

    virtual void emit_function_value(Value func_val) override;

    virtual void emit_instantiate_closure(const Function *function) override;
    virtual void emit_close_values(uint32_t num_values) override;

    virtual void emit_pop() override;

    virtual void emit_halt() override;

    virtual void *emit_push_handler_case(uint32_t num_handlers) override;
    virtual void emit_pop_handler_case() override;

    virtual void emit_raise_signal(uint32_t argc) override;

    virtual void emit_cons() override;
    virtual void emit_car() override;
    virtual void emit_cdr() override;
    virtual void emit_eq() override;
    virtual void emit_rplaca() override;
    virtual void emit_rplacd() override;
    virtual void emit_aref() override;
    virtual void emit_aset() override;

    virtual void emit_add() override;
    virtual void emit_add_1() override;
    virtual void emit_sub() override;
    virtual void emit_sub_1() override;
    virtual void emit_dup() override;

    virtual void emit_debug_trap() override;

    virtual void resolve_jump(void *branch_id, void *destination) override;
    virtual void resolve_jump_to_current(void *branch_id) override;
    void close_push_handler_case(void *id, void *destination);
    virtual void close_push_handler_case(void *id) override;
    virtual void backfill_label(void *branch_id, Value tag) override;

    virtual void *emit_jump() override;
    virtual void *emit_jump(void *destination) override;
    virtual void *emit_pop_jump_if_nil() override;
    virtual void *emit_pop_jump_if_nil(void *destination) override;

    virtual void push_labels() override;
    virtual void pop_labels() override;
    virtual void *user_label(Value tag) override;
    virtual void *internal_label(const char *label_tag) override;

    virtual bool label_to_offset(void *label, uint32_t &out_offset) override;

    virtual void finalize() override;
    virtual const std::vector<uint8_t> &bytecode() const override;
    virtual std::vector<uint8_t> &&move_bytecode() override;

    int32_t position() const;
    void map_range_to(size_t begin, size_t end, Value expr);


    template<typename T>
    void set_raw(size_t offset, T value)
    {
        assert(offset + sizeof(value) <= m_bytecode.size());
        T *slot = reinterpret_cast<T*>(m_bytecode.data() + offset);
        *slot = value;
    }

  private:
    bool get_label(Value tag, int32_t &out_offset);

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

    bool m_locked;
    std::vector<Range_Value_Pair> m_debug_map;

    std::vector<uint8_t> m_bytecode;
    std::vector<Label_Map> m_label_stack;
    std::list<Backfill_Info> m_backfills;
};

}
}

#endif
