#include "package.hpp"
#include "gc.hpp"

using namespace lisp;

void Package::gc_mark(GC &gc)
{
    for (auto &[k, v] : m_symbols)
    {
        gc.mark_value(v);
    }
}


Value Package::intern_symbol(const std::string &name)
{
    Value inherited;
    if (find_inherited(name, inherited))
    {
        return inherited;
    }

    auto it = m_symbols.find(name);
    if (it != m_symbols.end())
    {
        return it->second;
    }

    auto value = gc.alloc_object<Symbol>(name, Value::nil(), this);
    m_symbols[name] = value;
    return value;
}

bool Package::import_symbol(Value symbol_val)
{
    auto symbol = symbol_val.as_object()->symbol();
    if (symbol->package() == this)
    {
        return true;
    }

    auto it = m_symbols.find(symbol->name());
    if (it != m_symbols.end())
    {
        return false;
    }

    m_symbols[symbol->name()] = symbol_val;
    return true;
}

Value Package::export_symbol(const std::string &name)
{
    auto value = intern_symbol(name);
    m_exported_symbols[name] = value;
    return value;
}

bool Package::find_inherited(const std::string &name, Value &out_value)
{
    for (auto it = m_inherit_from.rbegin();
         it != m_inherit_from.rend();
         ++it)
    {
        if ((*it)->is_exported(name, &out_value))
        {
            return true;
        }
    }
    return false;
}

bool Package::is_exported(const std::string &name, Value *opt_out /*= nullptr*/) const
{
    auto it = m_exported_symbols.find(name);
    if (it != m_exported_symbols.end())
    {
        if (opt_out != nullptr)
        {
            *opt_out = it->second;
        }
        return true;
    }
    return false;
}

bool Package::find_symbol(const std::string &name, Value &out_value, Symbol_Location_Type *out_loc /*= nullptr*/)
{
    if (find_inherited(name, out_value))
    {
        if (out_loc)
        {
            *out_loc = Symbol_Location_Type::Inherited;
        }
        return true;
    }
    auto it = m_symbols.find(name);
    if (it != m_symbols.end())
    {
        out_value = it->second;
        if (out_loc)
        {
            *out_loc = is_exported(name)
                ? Symbol_Location_Type::External
                : Symbol_Location_Type::Internal;
        }
        return true;
    }
    return false;
}

Value Package::find_or_intern_symbol(const std::string &name)
{
    Value res;
    if (find_symbol(name, res))
    {
        return res;
    }
    return intern_symbol(name);
}

Package_Registry::Package_Registry()
    : m_current_package(nullptr)
{
    gc.register_marking_function([this](GC &gc) {this->gc_mark(gc);});
}

Package *Package_Registry::find_or_create(const std::string &name)
{
    auto package = find(name);
    if (package)
    {
        return package;
    }

    auto package_val = gc.alloc_object_unmanaged<Package>(name);
    package = package_val.as_object()->package();
    m_packages[name] = package;
    package->m_this_package = package_val;
    return package;
}

void Package_Registry::gc_mark(GC &gc)
{
    for (auto &[k, pkg] : m_packages)
    {
        pkg->gc_mark(gc);
    }
}
