#ifndef _LISPYBOI_VM_STATE_
#define _LISPYBOI_VM_STATE_

#include "defines.hpp"
#include "value.hpp"
#include "gc.hpp"
#include "util.hpp"

namespace lisp
{

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
        Exception(Value what)
            : what(what)
            , ctx(nullptr)
        {}

        Exception(Value what, const Signal_Context *ctx)
            : what(what)
            , ctx(ctx)
        {}

        Value what;
        const Signal_Context *ctx;
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

        Signal_Exception(Value what, const uint8_t *ip, Call_Frame *stack_top, Call_Frame *stack_bottom, const Signal_Context *ctx)
            : Exception(what, ctx)
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

    const uint8_t *execute(const uint8_t *ip)
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
    template<bool debuggable>
    const uint8_t *execute_impl(const uint8_t *ip);

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

extern VM_State *THE_LISP_VM;

}

#endif
