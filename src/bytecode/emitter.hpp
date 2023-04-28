#ifndef _LISPYBOI_EMITTER_
#define _LISPYBOI_EMITTER_

#include <vector>

namespace lisp::bytecode
{

class Emitter
{
  public:
    virtual ~Emitter() {}

    virtual void pp(const char *tag) {}
    virtual Emitter *of_same_type() const = 0;

    virtual void emit_push_value(Value value) = 0;
    virtual void emit_push_nil() = 0;
    virtual void emit_push_fixnum_0() = 0;
    virtual void emit_push_fixnum_1() = 0;

    virtual void emit_funcall(uint32_t argc) = 0;
    virtual void emit_gotocall(uint32_t argc) = 0;
    virtual void emit_apply(uint32_t argc) = 0;

    virtual void emit_return() = 0;


    virtual void emit_get_global(uint32_t idx) = 0;
    virtual void emit_get_local(uint32_t idx) = 0;
    virtual void emit_get_capture(uint32_t idx) = 0;

    virtual void emit_set_global(uint32_t idx) = 0;
    virtual void emit_set_local(uint32_t idx) = 0;
    virtual void emit_set_capture(uint32_t idx) = 0;

    virtual void emit_function_value(Value func_val) = 0;

    virtual void emit_instantiate_closure(const Function *function) = 0;
    virtual void emit_close_values(uint32_t num_values) = 0;

    virtual void emit_pop() = 0;

    virtual void emit_halt() = 0;

    virtual void emit_pop_handler_case() = 0;

    virtual void emit_raise_signal(uint32_t argc) = 0;

    virtual void emit_cons() = 0;
    virtual void emit_car() = 0;
    virtual void emit_cdr() = 0;
    virtual void emit_eq() = 0;
    virtual void emit_rplaca() = 0;
    virtual void emit_rplacd() = 0;
    virtual void emit_aref() = 0;
    virtual void emit_aset() = 0;

    virtual void emit_add() = 0;
    virtual void emit_add_1() = 0;
    virtual void emit_sub() = 0;
    virtual void emit_sub_1() = 0;
    virtual void emit_dup() = 0;

    virtual void emit_debug_trap() = 0;

    // returns the handler case ID
    virtual void *emit_push_handler_case(uint32_t num_handlers) = 0;
    // returns the branch id
    virtual void *emit_jump() = 0;
    // returns the branch id
    virtual void *emit_jump(void *destination) = 0;
    // returns the branch id
    virtual void *emit_pop_jump_if_nil() = 0;
    // returns the branch id
    virtual void *emit_pop_jump_if_nil(void *destination) = 0;

    virtual void resolve_jump(void *branch_id, void *destination) = 0;
    virtual void resolve_jump_to_current(void *branch_id) = 0;
    virtual void close_push_handler_case(void *handler_id) = 0;
    virtual void backfill_label(void *branch_id, Value tag) = 0;

    virtual void push_labels() = 0;
    virtual void pop_labels() = 0;
    virtual void *user_label(Value tag) = 0;
    // label_tag is merely a hint, it may be unused by the emitter.
    virtual void *internal_label(const char *label_tag = nullptr) = 0;

    virtual bool label_to_offset(void *label, uint32_t &out_offset) = 0;

    virtual void finalize() = 0;
    virtual const std::vector<uint8_t> &bytecode() const = 0;
    virtual std::vector<uint8_t> &&move_bytecode() = 0;

    //virtual int32_t position() const = 0;
    //virtual void lock() = 0;
    //virtual void map_range_to(size_t begin, size_t end, Value expr) = 0;
    //virtual bool get_label(Value tag, int32_t &out_offset) = 0;
    //virtual const std::vector<uint8_t> &bytecode() const = 0;
    //virtual std::vector<uint8_t> &&move_bytecode() = 0;

};

}

#endif
