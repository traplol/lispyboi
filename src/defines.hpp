#ifndef _LISPYBOI_DEFINES_
#define _LISPYBOI_DEFINES_

#if !defined(DEBUG)
#define DEBUG 0
#endif

#define XSTR(x) #x
#define STR(x) XSTR(x)
#if DEBUG > 1
#define ENSURE_VALUE(value, expr) do {                                  \
        if (!(expr)) {                                                  \
            fputs("ENSURE failed: '" STR(expr) "' was false.\n", stderr); \
            fputs("    " __FILE__ ":" STR(__LINE__) "\n", stderr);      \
            abort();                                    \
        }                                                               \
    } while (0)

#else
#define ENSURE_VALUE(value, expr) ((void)value)
#endif


#if DEBUG > 2
#if !defined(GC_DIAGNOSTICS)
#define GC_DIAGNOSTICS 1
#endif
#define FORCE_INLINE inline __attribute__((noinline))
#define FLATTEN __attribute__((noinline))

#elif DEBUG == 2
#define FORCE_INLINE inline __attribute__((noinline))
#define FLATTEN __attribute__((noinline))

#elif DEBUG == 1
#define FORCE_INLINE inline
#define FLATTEN

#elif DEBUG == 0
#define FORCE_INLINE inline __attribute__((always_inline))
#define FLATTEN __attribute__((flatten))
#define GC_NO_OPT 0
#define USE_COMPUTED_GOTOS 1

#else
#define FORCE_INLINE
#define FLATTEN
#error "Unknown DEBUG value"
#endif

#if !defined GC_DIAGNOSTICS
#define GC_DIAGNOSTICS 0
#endif

#if !defined(USE_COMPUTED_GOTOS)
#define USE_COMPUTED_GOTOS 0
#endif

#if !defined(GC_NO_OPT)
#define GC_NO_OPT 1
#endif

#if USE_COMPUTED_GOTOS == 0 && DEBUG > 1
#define PROFILE_OPCODE_PAIRS 1
#warning "Profiling opcode pairs is enabled."
#else
#define PROFILE_OPCODE_PAIRS 0
#endif

#endif
