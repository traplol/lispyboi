#ifndef _LISPYBOI_GC_
#define _LISPYBOI_GC_

#include <functional>
#include <chrono>

#include "defines.hpp"
#include "value.hpp"
#include "lisp_reference_types.hpp"
#include "runtime_globals.hpp"

namespace lisp
{

struct GC
{
    GC() {}

    ~GC()
    {
        // Not the most elegant way to GC everything at exit but it works.
        for (auto gen : m_generations)
        {
            for (auto ref : *gen)
            {
                ref->marked = false;
                m_recent_allocations.push_back(ref);
            }
        }
        sweep();
    }

    void init();

    struct Reference
    {
        enum Type : uint32_t
        {
            Cons,
            Object,
            Closure_Reference,
        };

        #if GC_NO_OPT
        static constexpr uint32_t MAGIC_CONSTANT = 0xFEEDFACE;
        uint32_t magic;
        #endif

        uint32_t size;

        uint32_t collections_survived : 20;
        uint32_t type : 10;
        uint32_t marking : 1;
        uint32_t marked : 1;

        #if GC_NO_OPT
        // This padding is required because we need data to be 8-byte aligned so that Value
        // is guaranteed to have 3 bits of tag storage.
        uint32_t _pad;
        #endif

        char data[0];

        template<typename T>
        T *as()
        {
            return reinterpret_cast<T*>(&data[0]);
        }
    };
    #if GC_NO_OPT
    static_assert(offsetof(Reference, magic) == 0);
    static_assert(offsetof(Reference, size) == 4);
    static_assert(offsetof(Reference, data) == 16);
    #else
    static_assert(offsetof(Reference, size) == 0);
    static_assert(offsetof(Reference, data) == 8);
    #endif


    Closure_Reference *make_closure_reference(Value *v);
    Value cons(Value car, Value cdr);
    void mark();
    size_t sweep();
    void maybe_mark_and_sweep();
    void mark_closure_reference(Closure_Reference *clos_ref);
    void mark_value(Value value);

    template<typename T, typename... Args>
    Value alloc_object(Args... args)
    {
        auto ref = allocate<true>(offsetof(Object, m_data) + sizeof(T));
        ref->type = Reference::Type::Object;
        auto obj = ref->as<Object>();
        obj->construct_data<T>(args...);
        return Value::wrap_object(obj);
    }

    template<typename T, typename... Args>
    Value alloc_object_unmanaged(Args... args)
    {
        auto ref = allocate<false>(offsetof(Object, m_data) + sizeof(T));
        ref->type = Reference::Type::Object;
        auto obj = ref->as<Object>();
        obj->construct_data<T>(args...);
        return Value::wrap_object(obj);
    }

    Value alloc_string(const char *str, Fixnum len);
    Value alloc_string(const std::string &str);

    bool paused() const
    {
        return m_is_paused;
    }

    bool pause()
    {
        auto tmp = m_is_paused;
        m_is_paused = true;
        return tmp;
    }

    void set_paused(bool new_val)
    {
        m_is_paused = new_val;
    }

    using Mark_Function = std::function<void(GC&)>;

    void register_marking_function(Mark_Function func)
    {
        m_mark_functions.push_back(func);
    }

    inline void mark_symbol(Symbol *symbol)
    {
        mark_value(symbol->m_function);
    }

    inline void mark_closure(Closure *closure)
    {
        for (auto clos_ref : closure->captures())
        {
            mark_closure_reference(clos_ref);
        }
    }

    //inline void mark_simple_array(Simple_Array *simple_array);

    inline void mark_simple_array(Simple_Array *simple_array)
    {
        const auto t = simple_array->element_type();
        if (t != g.s_CHARACTER && t != g.s_FIXNUM)
        {
            for (Fixnum i = 0; i < simple_array->size(); ++i)
            {
                mark_value(simple_array->at(i));
            }
        }
    }

    inline void mark_structure(Structure *structure)
    {
        for (Fixnum i = 0; i < structure->num_slots(); ++i)
        {
            mark_value(structure->slot_value(i));
        }
    }

    size_t mark_and_sweep()
    {
        #if GC_DIAGNOSTICS > 0
        fprintf(stderr, "New mark started.\n");
        #endif
        m_marked = 0;
        mark();
        #if GC_DIAGNOSTICS > 0
        fprintf(stderr, "Mark phase finished.\n");
        fprintf(stderr, "Marked %zu\n", m_marked);
        #endif
        return sweep();
    }

    template<typename... Args>
    Value list(Args... args)
    {
        if constexpr (sizeof...(args) == 0)
        {
            return Value::nil();
        }

        std::array<Ptr_Handle, sizeof...(args)> arg_handles = {pin_value(args)...};
        std::array<Value, sizeof...(args)> arg_values = {args...};

        auto head = cons(arg_values[0], Value::nil());
        auto head_handle = pin_value(head);
        unpin_value(arg_handles[0]);
        auto curr = head;

        for (size_t i = 1; i < arg_values.size(); ++i)
        {
            set_cdr(curr, cons(arg_values[i], Value::nil()));
            unpin_value(arg_handles[i]);
            curr = cdr(curr);
        }

        unpin_value(head_handle);
        return head;
    }

    struct Ptr_Handle
    {
        void *ptr;
    };

    Ptr_Handle pin_value(Value value)
    {
        if (value.is_garbage_collected())
        {
            return {m_pinned_values.push_back(value)};
        }
        return {nullptr};
    }

    void unpin_value(Ptr_Handle &handle)
    {
        if (handle.ptr)
        {
            auto node = static_cast<Pinned_Values_List::Node*>(handle.ptr);
            handle.ptr = nullptr;
            m_pinned_values.unlink_and_free(node);
        }
    }

    size_t get_consed() const
    {
        return m_total_consed;
    }

    size_t get_freed() const
    {
        return m_total_freed;
    }

    size_t get_time_spent_in_gc() const
    {
        return m_time_spent_in_gc;
    }

    size_t get_times_gc_has_run() const
    {
        return m_times_gc_has_run;
    }

    size_t get_collect_threshold() const
    {
        return m_gc_collect_threshold;
    }

    void set_collect_threshold(size_t new_threshold)
    {
        m_gc_collect_threshold = new_threshold;
    }

  private:

    Reference *ptr_to_ref(void *ptr)
    {
        if (!ptr)
        {
            return nullptr;
        }

        auto p = reinterpret_cast<uint8_t*>(ptr) - offsetof(Reference, data);

        auto ref = reinterpret_cast<Reference*>(p);
        #if GC_NO_OPT
        if (ref->magic != Reference::MAGIC_CONSTANT)
        {
            return nullptr;
        }
        #endif
        return ref;
    }

    Reference *value_to_ref(Value val)
    {
        if (!val.is_garbage_collected())
        {
            return nullptr;
        }

        auto ptr = val.unwrap_pointer();
        return ptr_to_ref(ptr);
    }


    template<bool is_managed>
    Reference *allocate(size_t size)
    {
        if constexpr (is_managed)
        {
            m_total_consed += size;
            m_allocated_since_last_gc += size;
            maybe_mark_and_sweep();
        }

        Reference *ref = nullptr;
        if constexpr (is_managed)
        {
            auto &bin = get_bin(size);
            for (size_t i = 0; i < bin.size(); ++i)
            {
                if (bin[i]->size >= size)
                {
                    ref = bin[i];
                    bin[i] = bin.back();
                    bin.pop_back();
                    break;
                }
            }
        }

        if (!ref)
        {
            ref = static_cast<Reference*>(::operator new(offsetof(Reference, data) + size));
            ref->size = size;
        }

        #if GC_NO_OPT
        ref->magic = Reference::MAGIC_CONSTANT;
        #endif
        ref->collections_survived = 0;
        ref->marking = false;
        ref->marked = false;

        if constexpr (is_managed)
        {
            m_recent_allocations.push_back(ref);
        }

        return ref;
    }

    using Generation = std::vector<Reference*>;

    Generation &current_generation()
    {
        if (m_generations.size() == 0)
        {
            make_new_generation();
        }
        return *m_generations.back();
    }

    void make_new_generation()
    {
        m_generations.push_back(new Generation);
    }

    std::vector<Reference*> &get_bin(size_t size)
    {
        if (size > SMALL_BINS_SIZE)
        {
            return m_free_large_bin;
        }
        return m_free_small_bins[size];
    }

    // attempt to keep GC from running many times in succession
    //static constexpr auto GC_COOLDOWN_THRESHOLD = GC_WARMUP_THRESHOLD * 0.06;
    static constexpr auto NEW_GENERATION_THRESHOLD = 150000; // objects per generation
    // how many GC runs does a Reference need to survive before being moved to the current generation?
    static constexpr auto GENERATIONAL_SURVIVOR_THRESHOLD = 2;
    static constexpr auto SMALL_BINS_SIZE = 256;

    std::vector<Generation*> m_generations;
    std::vector<Reference*> m_recent_allocations;
    //std::vector<Reference*> m_free_references;
    std::vector<std::vector<Reference*>> m_free_small_bins;
    std::vector<Reference*> m_free_large_bin;
    struct Pinned_Values_List
    {
        struct Node
        {
            Value value;
            Node *next;
            Node *prev;
        };
        Pinned_Values_List()
            : m_head(nullptr)
            , m_tail(nullptr)
        {
            constexpr size_t n = 1000;
            m_free_node_pool.reserve(n);
            for (size_t i = 0; i < n; ++i)
            {
                m_free_node_pool.push_back(new Node {Value::wrap_fixnum(0), nullptr, nullptr});
            }
        }

        Node *head()
        {
            return m_head;
        }

        Node *tail()
        {
            return m_tail;
        }

        Node *push_back(Value value)
        {
            if (m_head)
            {
                m_tail->next = alloc_node(value, m_tail);
                m_tail = m_tail->next;
            }
            else
            {
                m_head = alloc_node(value, nullptr);
                m_tail = m_head;
            }
            return m_tail;
        }

        void unlink_and_free(Node *node)
        {
            auto prev = node->prev;
            auto next = node->next;
            if (prev)
            {
                prev->next = next;
            }
            if (next)
            {
                next->prev = prev;
            }
            if (node == m_head)
            {
                m_head = next;
            }
            else if (node == m_tail)
            {
                m_tail = prev;
            }
            free_node(node);
        }

      private:
        inline Node *alloc_node(Value value, Node *prev)
        {
            if (m_free_node_pool.empty())
            {
                return new Node {value, nullptr, prev};
            }
            auto node = m_free_node_pool.back();
            new (node) Node{value, nullptr, prev};
            m_free_node_pool.pop_back();
            return node;
        }

        inline void free_node(Node *node)
        {
            m_free_node_pool.push_back(node);
        }

        Node *m_head;
        Node *m_tail;
        std::vector<Node*> m_free_node_pool;
    } m_pinned_values;
    std::vector<Mark_Function> m_mark_functions;
    size_t m_marked;
    size_t m_total_consed;
    size_t m_total_freed;
    size_t m_allocated_since_last_gc;
    size_t m_gc_collect_threshold;
    size_t m_time_spent_in_gc;
    size_t m_times_gc_has_run;
    bool m_is_paused;
};

extern GC gc;

}

#define GC_GUARD()                              \
    auto __gc_guard__paused = lisp::gc.pause()  \

#define GC_UNGUARD() lisp::gc.set_paused(__gc_guard__paused)


#endif
