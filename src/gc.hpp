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
    GC()
        : m_marked(0)
        , m_total_consed(0)
        , m_total_freed(0)
        , m_allocated_since_last_gc(0)
        , m_gc_collect_threshold(1 * 1024 * 1024) // 1 MiB
        , m_time_spent_in_gc(0)
        , m_times_gc_has_run(0)
        , m_is_paused(true)
    {
        m_free_small_bins.resize(SMALL_BINS_SIZE);
    }

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


    Closure_Reference *make_closure_reference(Value *v)
    {
        auto ref = allocate<true>(sizeof(Closure_Reference));
        ref->type = Reference::Type::Closure_Reference;
        auto clos_ref = ref->as<Closure_Reference>();
        new (clos_ref) Closure_Reference{v};
        return clos_ref;
    }

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

    Value cons(Value car, Value cdr)
    {
        auto ref = allocate<true>(sizeof(Cons));
        ref->type = Reference::Type::Cons;
        auto cons = ref->as<Cons>();
        new (cons) Cons{car, cdr};
        return Value::wrap_cons(cons);
    }

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

    void mark()
    {
        auto curr = m_pinned_values.head();
        while (curr)
        {
            mark_value(curr->value);
            curr = curr->next;
        }

        for (auto it : m_mark_functions)
        {
            it(*this);
        }
    }

    inline void mark_symbol(Symbol *symbol)
    {
        mark_value(symbol->m_function);
    }

    void mark_closure_reference(Closure_Reference *clos_ref)
    {
        while (clos_ref)
        {
            auto ref = ptr_to_ref(clos_ref);
            if (ref && !ref->marking)
            {
                ref->marking = true;

                mark_value(clos_ref->value());

                ref->collections_survived++;
                ref->marked = true;
                ref->marking = false;
                m_marked++;
            }
            clos_ref = clos_ref->next;
        }
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

    void mark_value(Value value)
    {
        auto ref = value_to_ref(value);
        if (!ref)
        {
            return;
        }

        if (ref->marking ||
            (ref->marked && ref->collections_survived <= GENERATIONAL_SURVIVOR_THRESHOLD))
        {
            return;
        }

        ref->marking = true;

        #if GC_DIAGNOSTICS > 1
        printf("Marking: %s: %p\n", repr(value).c_str(), (void*)value.bits());
        #endif
        if (value.is_cons())
        {
            auto cur = value;
            while (!cur.is_nil())
            {
                // This appears to be horribly recursive, but this marking function doesn't try to mark
                // values that are already being marked or have been marked already. This makes the
                // usual list case fairly linear and the only real risk of exceeding the recursion limit
                // would be from a very heavily left-leaning tree.
                mark_value(car(cur));
                auto cur_cons_ref = value_to_ref(cur);
                cur_cons_ref->marked = true;
                cur_cons_ref->collections_survived++;
                //mark_value(cur);
                cur = cdr(cur);
                if (!cur.is_cons() && !cur.is_nil())
                {
                    mark_value(cur);
                    break;
                }
            }
        }
        else
        {
            auto obj = value.as_object();
            switch (obj->type())
            {
                case Object_Type::Symbol:
                    mark_symbol(obj->symbol());
                    break;
                case Object_Type::Closure:
                    mark_closure(obj->closure());
                    break;
                case Object_Type::Package:
                    break;
                case Object_Type::File_Stream:
                    break;
                case Object_Type::Simple_Array:
                    // @TODO: Optimize scanning array if it's a string?
                    mark_simple_array(obj->simple_array());
                    break;
                case Object_Type::System_Pointer:
                    break;
                case Object_Type::Structure:
                    mark_structure(obj->structure());
                    break;
                case Object_Type::Float:
                    break;
            }
        }

        ref->collections_survived++;
        ref->marked = true;
        ref->marking = false;
        m_marked++;
    }

    size_t sweep()
    {
        #if GC_DIAGNOSTICS > 0
        fprintf(stderr, "Running sweep... %zu ", m_recent_allocations.size());
        #endif
        auto &current_gen = current_generation();

        #if GC_DIAGNOSTICS > 0
        size_t moved_to_generation = 0;
        #endif
        size_t freed = 0;
        for (size_t i = 0; i < m_recent_allocations.size();)
        {
            auto r = m_recent_allocations[i];
            if (r->marked == false)
            {
                switch (r->type)
                {
                    case Reference::Type::Cons:
                        break;
                    case Reference::Type::Object:
                        r->as<Object>()->~Object();
                        break;
                    case Reference::Type::Closure_Reference:
                        break;
                }
                #if DEBUG > 1
                memset(r->data, 0xCC, r->size);
                #endif
                get_bin(r->size).push_back(r);
                m_recent_allocations[i] = m_recent_allocations.back();
                m_recent_allocations.pop_back();
                freed += r->size;
            }
            else if (r->collections_survived >= GENERATIONAL_SURVIVOR_THRESHOLD)
            {
                r->marked = false;
                current_gen.push_back(r);
                m_recent_allocations[i] = m_recent_allocations.back();
                m_recent_allocations.pop_back();
                #if GC_DIAGNOSTICS > 0
                moved_to_generation++;
                #endif
            }
            else
            {
                r->marked = false;
                i++;
            }
        }

        #if GC_DIAGNOSTICS > 0
        fprintf(stderr, " Moved: %zu, Freed: %zu, Generation (%zu) Size: %zu\n",
                moved_to_generation, freed, m_generations.size(), current_gen.size());
        #endif

        if (current_gen.size() >= NEW_GENERATION_THRESHOLD)
        {
            make_new_generation();
        }
        m_total_freed += freed;
        return freed;
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

    FORCE_INLINE
    void maybe_mark_and_sweep()
    {
        if (!m_is_paused && m_allocated_since_last_gc > m_gc_collect_threshold)
        {
            // If a sweep doesn't free enough then to minimize successive garbage collections
            // we'll revert to a no longer warmed-up state which will only switch back when
            // there are enough recent allocations.
            auto start_time = std::chrono::high_resolution_clock::now();
            if (mark_and_sweep() < m_gc_collect_threshold * 0.3)
            {
                m_gc_collect_threshold *= 1.3;
            }
            auto end_time = std::chrono::high_resolution_clock::now();
            auto duration = std::chrono::duration_cast<std::chrono::microseconds>(end_time - start_time).count();
            m_time_spent_in_gc += duration;
            m_times_gc_has_run++;
            m_allocated_since_last_gc = 0;
        }
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
