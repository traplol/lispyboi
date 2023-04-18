#include "gc.hpp"

using namespace lisp;

Closure_Reference *GC::make_closure_reference(Value *v)
{
    auto ref = allocate<true>(sizeof(Closure_Reference));
    ref->type = Reference::Type::Closure_Reference;
    auto clos_ref = ref->as<Closure_Reference>();
    new (clos_ref) Closure_Reference{v};
    return clos_ref;
}

Value GC::cons(Value car, Value cdr)
{
    auto ref = allocate<true>(sizeof(Cons));
    ref->type = Reference::Type::Cons;
    auto cons = ref->as<Cons>();
    new (cons) Cons{car, cdr};
    return Value::wrap_cons(cons);
}

void GC::mark()
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

void GC::mark_closure_reference(Closure_Reference *clos_ref)
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

void GC::mark_value(Value value)
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

size_t GC::sweep()
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

void GC::maybe_mark_and_sweep()
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

void GC::init()
{
    m_marked = 0;
    m_total_consed = 0;
    m_total_freed = 0;
    m_allocated_since_last_gc = 0;
    m_gc_collect_threshold = 1 * 1024 * 1024; // 1 MiB
    m_time_spent_in_gc = 0;
    m_times_gc_has_run = 0;
    m_is_paused = true;

    m_free_small_bins.resize(SMALL_BINS_SIZE);
}
