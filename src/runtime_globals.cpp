#include "gc.hpp"
#include "runtime_globals.hpp"

using namespace lisp;

Runtime_Globals::Runtime_Globals()
{
    gc.register_marking_function([this](GC &gc) {this->gc_mark(gc);});
}

void Runtime_Globals::gc_mark(GC &gc)
{
    for (auto it : global_value_slots)
    {
        gc.mark_value(it);
    }

    for (auto it : literal_object_slots)
    {
        gc.mark_value(it);
    }

    for (auto &[k, val] : macros)
    {
        gc.mark_value(val);
    }
}
