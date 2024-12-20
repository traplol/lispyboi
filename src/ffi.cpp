#include "ffi.hpp"

#include <stdlib.h>
#include <stdio.h>
#include <dlfcn.h>

void *ffi::open(const char *path)
{
    return dlopen(path, RTLD_NOW);
}

int ffi::close(void *handle)
{
    return dlclose(handle);
}

void *ffi::getsym(void *handle, const char *symbol_name)
{
    return dlsym(handle, symbol_name);
}

void *ffi::alloc_mem(size_t size)
{
    return malloc(size);
}

void *ffi::calloc_mem(size_t size)
{
    return calloc(size, 1);
}

void ffi::free_mem(void *ptr)
{
    free(ptr);
}

template<typename ...Args>
inline static
void *do_call(void *f, Args... args)
{
    auto func = reinterpret_cast<void*(*)(Args...)>(f);
    return func(args...);
}

#define M(n) (args[n])
void *ffi::call(void *f, void **args, uint32_t nargs)
{
    switch (nargs) {
        case  0: return do_call(f);
        case  1: return do_call(f, M(0));
        case  2: return do_call(f, M(0), M(1));
        case  3: return do_call(f, M(0), M(1), M(2));
        case  4: return do_call(f, M(0), M(1), M(2), M(3));
        case  5: return do_call(f, M(0), M(1), M(2), M(3), M(4));
        case  6: return do_call(f, M(0), M(1), M(2), M(3), M(4), M(5));
        case  7: return do_call(f, M(0), M(1), M(2), M(3), M(4), M(5), M(6));
        case  8: return do_call(f, M(0), M(1), M(2), M(3), M(4), M(5), M(6), M(7));
        case  9: return do_call(f, M(0), M(1), M(2), M(3), M(4), M(5), M(6), M(7), M(8));
        case 10: return do_call(f, M(0), M(1), M(2), M(3), M(4), M(5), M(6), M(7), M(8), M(9));
        case 11: return do_call(f, M(0), M(1), M(2), M(3), M(4), M(5), M(6), M(7), M(8), M(9), M(10));
        case 12: return do_call(f, M(0), M(1), M(2), M(3), M(4), M(5), M(6), M(7), M(8), M(9), M(10), M(11));
        case 13: return do_call(f, M(0), M(1), M(2), M(3), M(4), M(5), M(6), M(7), M(8), M(9), M(10), M(11), M(12));
        case 14: return do_call(f, M(0), M(1), M(2), M(3), M(4), M(5), M(6), M(7), M(8), M(9), M(10), M(11), M(12), M(13));
        case 15: return do_call(f, M(0), M(1), M(2), M(3), M(4), M(5), M(6), M(7), M(8), M(9), M(10), M(11), M(12), M(13), M(14));
    }
    fprintf(stderr, "ffi call with more than 16 arguments is no supported\n");
    return nullptr;
}

void ffi::call_void(void *f, void **args, uint32_t nargs)
{
    switch (nargs) {
        case  0: do_call(f); break;
        case  1: do_call(f, M(0)); break;
        case  2: do_call(f, M(0), M(1)); break;
        case  3: do_call(f, M(0), M(1), M(2)); break;
        case  4: do_call(f, M(0), M(1), M(2), M(3)); break;
        case  5: do_call(f, M(0), M(1), M(2), M(3), M(4)); break;
        case  6: do_call(f, M(0), M(1), M(2), M(3), M(4), M(5)); break;
        case  7: do_call(f, M(0), M(1), M(2), M(3), M(4), M(5), M(6)); break;
        case  8: do_call(f, M(0), M(1), M(2), M(3), M(4), M(5), M(6), M(7)); break;
        case  9: do_call(f, M(0), M(1), M(2), M(3), M(4), M(5), M(6), M(7), M(8)); break;
        case 10: do_call(f, M(0), M(1), M(2), M(3), M(4), M(5), M(6), M(7), M(8), M(9)); break;
        case 11: do_call(f, M(0), M(1), M(2), M(3), M(4), M(5), M(6), M(7), M(8), M(9), M(10)); break;
        case 12: do_call(f, M(0), M(1), M(2), M(3), M(4), M(5), M(6), M(7), M(8), M(9), M(10), M(11)); break;
        case 13: do_call(f, M(0), M(1), M(2), M(3), M(4), M(5), M(6), M(7), M(8), M(9), M(10), M(11), M(12)); break;
        case 14: do_call(f, M(0), M(1), M(2), M(3), M(4), M(5), M(6), M(7), M(8), M(9), M(10), M(11), M(12), M(13)); break;
        case 15: do_call(f, M(0), M(1), M(2), M(3), M(4), M(5), M(6), M(7), M(8), M(9), M(10), M(11), M(12), M(13), M(14)); break;
        default:
            fprintf(stderr, "ffi call with more than 16 arguments is no supported\n");
    }
}
