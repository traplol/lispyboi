#ifndef _LISPYBOI_PACKAGE_
#define _LISPYBOI_PACKAGE_

#include <unordered_map>
#include <vector>

#include "value.hpp"

namespace lisp
{

struct GC;

struct Package
{
    enum class Symbol_Location_Type
    {
        Internal,
        External,
        Inherited
    };

    using Name_Symbol_Map = std::unordered_map<std::string, Value>;

    const std::string &name() const
    {
        return m_name;
    }

    const Name_Symbol_Map &symbols() const
    {
        return m_symbols;
    }

    const Name_Symbol_Map &exported_symbols() const
    {
        return m_exported_symbols;
    }

    void inherit(Package *pkg)
    {
        if (pkg)
        {
            m_inherit_from.push_back(pkg);
        }
    }

    Value intern_symbol(const std::string &name);
    bool import_symbol(Value symbol_val);
    Value export_symbol(const std::string &name);
    bool find_inherited(const std::string &name, Value &out_value);
    bool is_exported(const std::string &name, Value *opt_out = nullptr) const;
    bool find_symbol(const std::string &name, Value &out_value, Symbol_Location_Type *out_loc = nullptr);
    Value find_or_intern_symbol(const std::string &name);

    Value as_lisp_value()
    {
        return m_this_package;
    }

    Package(const std::string &name) : m_name(name) {}
  private:
    friend struct Package_Registry;

    void gc_mark(GC &gc);

    std::string m_name;
    Name_Symbol_Map m_symbols;
    Name_Symbol_Map m_exported_symbols;
    std::vector<Package*> m_inherit_from;
    Value m_this_package;
};

struct Package_Registry
{
    Package_Registry();

    Package *find(const std::string &name) const
    {
        auto it = m_packages.find(name);
        if (it != m_packages.end())
        {
            return it->second;
        }
        return nullptr;
    }

    Package *find_or_create(const std::string &name);

    Package *current() const
    {
        return m_current_package;
    }

    void in_package(Package *p)
    {
        m_current_package = p;
    }

    bool package_exists(const std::string &name) const
    {
        return m_packages.find(name) != m_packages.end();
    }

    void alias_package(const std::string &alias, Package *to)
    {
        auto it = m_packages.find(alias);
        if (it != m_packages.end() && it->second != to)
        {
            fprintf(stderr, "WARNING: Overwriting package %s with alias %s -> %s\n",
                    it->second->name().c_str(), alias.c_str(), to->name().c_str());
        }
        m_packages[alias] = to;
    }

    const std::unordered_map<std::string, Package*> &packages() const
    {
        return m_packages;
    }

  private:
    void gc_mark(GC &gc);
    std::unordered_map<std::string, Package*> m_packages;
    Package *m_current_package;
};

}

#endif
