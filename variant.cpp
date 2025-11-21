#include <iostream>
#include <cassert>
#include <cxxabi.h>

namespace details
{
    template<typename T, typename Replace, typename With> struct replace;
    template<typename T, typename Replace, typename With> using replace_t = typename replace<T, Replace, With>::type;

    template<typename T, typename Replace, typename With>
    struct replace { using type = T; };

    template<typename Replace, typename With>
    struct replace<Replace, Replace, With> { using type = With; };

    template<typename T, typename Replace, typename With>
    struct replace<const T, Replace, With> { using type = const replace_t<T, Replace, With>; };

    template<typename T, typename Replace, typename With>
    struct replace<T*, Replace, With> { using type = replace_t<T, Replace, With>*; };

    template<typename T, typename Replace, typename With>
    struct replace<T&, Replace, With> { using type = replace_t<T, Replace, With>&; };

    template<typename T, typename Replace, typename With>
    struct replace<T&&, Replace, With> { using type = replace_t<T, Replace, With>&&; };

    template<typename T, typename Replace, typename With>
    struct replace<const T*, Replace, With> { using type = const replace_t<T, Replace, With>*; };

    template<typename T, typename Replace, typename With>
    struct replace<const T&, Replace, With> { using type = const replace_t<T, Replace, With>&; };

    template<typename... Ts, typename Replace, typename With>
    struct replace<std::variant<Ts...>, Replace, With>
    {
        using type = std::variant<replace_t<Ts, Replace, With>...>;
    };

    template<template <typename...> typename F, typename Replace, typename With, typename... Ts>
    struct replace<F<Ts...>, Replace, With>
    {
        using type = F<replace_t<Ts, Replace, With>...>;
    };
}

struct _ {};

namespace details
{
    template<typename With, template <typename> typename box, typename... Ts>
    struct variant_rewriter
    {
    private:
        template<typename T>
        struct impl
        {
            // Only box when it has a recursive flag
            using type = std::conditional_t<
                std::is_same_v<
                    replace_t<T, _, With>,
                    T
                >,
                T,
                box<replace_t<T, _, With> >
            >;
        };

    public:
        using type = std::variant<typename impl<Ts>::type...>;
    };
}

template<typename T>
struct box
{
    using Self = box<T>;
    T * m_p;

    box() = delete;
    box(Self&&) = default;
    box(Self const& rhs) = delete;
    box(T* p) : m_p(p) {}
    ~box() { if (m_p) delete m_p; };

    T& get() { return *m_p; }
    T const& get() const { return *m_p; }
};

template<typename... Ts>
struct MakeVariant
{
    struct type;

    static_assert((std::is_copy_constructible_v<Ts> && ...), "Variant types must be copy constructible");

public:
    // use std::variant, but add Boxing to allow types to be recursive
    using base_type = typename details::variant_rewriter<type, box, Ts...>::type;

private:
    struct copy_visitor
    {
        template<typename T>
        base_type operator()(T const& rhs) const
        {
            return base_type(rhs);
        }

        template<typename T>
        base_type operator()(box<T> const& rhs) const
        {
            return base_type(box<T>(new T(rhs.get())));
        }
    };

    struct move_visitor
    {
        template<typename T>
        base_type operator()(T&& rhs) const
        {
            return base_type(std::move(rhs));
        }

        template<typename T>
        base_type operator()(box<T>&& rhs) const
        {
            return base_type(std::move(box<T>(rhs.m_p)));
        }
    };

    // copy but making Box have value semantics
    static base_type copy(base_type const& rhs)
    {
        return std::visit(copy_visitor(), rhs);
    }

    // move but making Box have value semantics
    static base_type move(base_type&& rhs)
    {
        return std::visit(move_visitor(), std::move(rhs));
    }

public:
    struct type
    {
    private:
        base_type m_impl;

    public:
        type() = delete;
        type(type const& rhs) : m_impl(copy(rhs.m_impl)) {}
        type(type&& rhs) : m_impl(move(std::move(rhs.m_impl))) {}
        ~type() = default;

        template<typename Rhs, typename std::enable_if<std::is_constructible<base_type, Rhs const&>::value, int>::type = 0>
        type(Rhs const& rhs) : m_impl(rhs) {}

        template<typename Rhs, typename std::enable_if<std::is_constructible<base_type, Rhs &&>::value, int>::type = 0>
        type(Rhs && rhs) : m_impl(std::move(rhs)) {}

        type& operator=(type const&) = default;

        int which() const { return m_impl.index(); }
    };
};

template<typename T>
struct BinOp_
{
    using Self = BinOp_<T>;

    BinOp_(T const& lhs_, T const& rhs_) : lhs(lhs_), rhs(rhs_) {}
    BinOp_(T&& lhs_, T&& rhs_) : lhs(std::move(lhs_)), rhs(std::move(rhs_)) {}

    T lhs, rhs;
};

struct handle {
    char* p;
    handle(char* ptr) : p(ptr) { }
    ~handle() { std::free(p); }
};

std::string demangle(const char* name) {

    int status = -4; // some arbitrary value to eliminate the compiler warning

    handle result( abi::__cxa_demangle(name, NULL, NULL, &status) );

    return (status==0) ? result.p : name ;
}

int main(int argc, char *argv[])
{
    static_assert(std::is_same_v<MakeVariant<int>::base_type, std::variant<int> >, "arse");
    static_assert(std::is_same_v<MakeVariant<int, bool>::base_type, std::variant<int, bool> >, "arse");

    MakeVariant<int>::type v1(2);
    MakeVariant<int, bool>::type v2(2);
    MakeVariant<int, bool>::type v3(false);

    assert(v1.which() == 0);
    assert(v2.which() == 0);
    assert(v3.which() == 1);

    using Variant2 = MakeVariant<int, BinOp_<_>>::type;
    using BinOp    = BinOp_<Variant2>;
    
    std::cerr << demangle(typeid(Variant2).name()) << std::endl;
    std::cerr << demangle(typeid(MakeVariant<int, BinOp_<_>>::base_type).name()) << std::endl;

    Variant2 x(2);
    Variant2 y(x);
    return 0;
}
