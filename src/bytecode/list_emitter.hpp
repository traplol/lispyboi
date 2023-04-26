#ifndef _LISPYBOI_LIST_EMITTER_
#define _LISPYBOI_LIST_EMITTER_

#include <list>
#include <vector>
#include <unordered_map>
#include <assert.h>

#include "../value.hpp"
#include "../util.hpp"
#include "opcode.hpp"
#include "emitter.hpp"

namespace lisp
{

namespace bytecode
{


struct Bytecode_List
{
    Bytecode_List()
        : prev(nullptr)
        , next(nullptr)
        , opcode(Opcode::op_halt)
        , is_label(false)
    {}
    Bytecode_List *prev;
    Bytecode_List *next;

    Opcode opcode;
    bool is_label;
    union
    {
        Value value;
        uint32_t num;
        const Function *function;
        Bytecode_List *destination;
    } operands[3];
};

class List_Emitter : public Emitter
{
  public:

    List_Emitter()
        : m_head(nullptr)
        , m_current(nullptr)
        , m_label_id(0)
    {}

    void pp();
    void do_tests();

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

    virtual void emit_debug_trap() override;

    virtual void *emit_jump() override;
    virtual void *emit_jump(void *destination) override;
    virtual void *emit_pop_jump_if_nil() override;
    virtual void *emit_pop_jump_if_nil(void *destination) override;

    virtual void push_labels() override;
    virtual void pop_labels() override;
    virtual void *user_label(Value tag) override;
    virtual void *internal_label() override;

    virtual void resolve_jump(void *branch_id, void *destination) override;
    virtual void resolve_jump_to_current(void *branch_id) override;
    virtual void close_push_handler_case(void *handler_case_id) override;
    virtual void backfill_label(void *branch_id, Value tag) override;

    Bytecode_List *current() const;
    void *get_label(Value tag);

  private:
    void append(Bytecode_List *node);

    Bytecode_List *m_head;
    Bytecode_List *m_current;
    int32_t m_label_id;

    using Label_Map = std::unordered_map<Value, Bytecode_List*>;
    std::vector<Label_Map> m_label_stack;
    std::unordered_map<int32_t, Bytecode_List*> m_internal_labels;
};

}
}

#endif
