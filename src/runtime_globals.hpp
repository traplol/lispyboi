#ifndef _LISPYBOI_RUNTIME_GLOBALS_
#define _LISPYBOI_RUNTIME_GLOBALS_

#include "value.hpp"
#include "package.hpp"

namespace lisp
{
struct Package;
struct GC;

struct Runtime_Globals
{
    void init();
    void gc_mark(GC &gc);

    static inline const char *kernel_str() { return "KERNEL"; }
    static inline const char *keyword_str() { return "KEYWORD"; }
    static inline const char *core_str() { return "LISPYBOI"; }
    static inline const char *user_str() { return "LISPYBOI-USER"; }

    Package *kernel()
    {
        return packages.find_or_create(kernel_str());
    }

    Package *keyword()
    {
        return packages.find_or_create(keyword_str());
    }

    Package *core()
    {
        return packages.find_or_create(core_str());
    }

    Package *user()
    {
        return packages.find_or_create(user_str());
    }

    Value get_keyword(const std::string &symbol_name)
    {
        return keyword()->intern_symbol(symbol_name);
    }

    Value get_symbol(const std::string &symbol_name)
    {
        return packages.current()->find_or_intern_symbol(symbol_name);
    }

    void resize_globals(size_t newsize)
    {
        global_value_slots.resize(newsize);
    }

    Value s_T;
    Value s_IF;
    Value s_OR;
    Value s_FIXNUM;
    Value s_FLOAT;
    Value s_CONS;
    Value s_LIST;
    Value s_CHARACTER;
    Value s_SYMBOL;
    Value s_STRING;
    Value s_FUNCTION;
    Value s_BOOLEAN;
    Value s_STRUCTURE;
    Value s_PACKAGE;
    Value s_FILE_STREAM;
    Value s_SIMPLE_ARRAY;
    Value s_SYSTEM_POINTER;
    Value s_QUOTE;
    Value s_QUASIQUOTE;
    Value s_UNQUOTE;
    Value s_UNQUOTE_SPLICING;
    Value s_TYPE_ERROR;
    Value s_SIMPLE_ERROR;
    Value s_aOPTIONAL;
    Value s_aREST;
    Value s_aBODY;
    Value s_aWHOLE;

    Value s_NULL;
    Value s_DIVIDE_BY_ZERO_ERROR;
    Value s_INDEX_OUT_OF_BOUNDS_ERROR;
    Value s_END_OF_FILE;
    Value s_BIT;
    Value s_OVERWRITE;
    Value s_APPEND;
    Value s_READ;
    Value s_MARSHAL_ERROR;
    Value s_BEGINNING;
    Value s_END;
    Value s_CURRENT;

    Value s_pplus;
    Value s_pminus;
    Value s_pLAMBDA;
    Value s_pCAR;
    Value s_pCDR;
    Value s_pCONS;
    Value s_pEQ;
    Value s_pRPLACA;
    Value s_pRPLACD;
    Value s_pSETQ;
    Value s_pAREF;
    Value s_pASET;
    Value s_pDEBUGGER;
    Value s_pAPPLY;
    Value s_pFUNCALL;
    Value s_pTAGBODY;
    Value s_pGO;
    Value s_pSIGNAL;
    Value s_pHANDLER_CASE;
    Value s_pDEFINE_MACRO;

    Package_Registry packages;
    std::vector<Value> global_value_slots;
    std::vector<Value> literal_object_slots;
    std::unordered_map<Symbol*, Value> macros;

    struct Debugger
    {
        enum Command
        {
            Continue,
            Step_Into,
            Step_Over,
        };

        Debugger()
            : addr0(nullptr)
            , addr1(nullptr)
            , command(Command::Continue)
            , breaking(false)
        {}

        const uint8_t *addr0;
        const uint8_t *addr1;
        Command command;
        bool breaking;
    } debugger;
};

extern Runtime_Globals g;

}

#endif
