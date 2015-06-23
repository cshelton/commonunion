#include <iostream>
#include <typeinfo>

using namespace std;

#define defstruct(name) struct name { int foo() { cout << #name << endl; return 0; } };

defstruct(A) defstruct(B) defstruct(C) defstruct(D) defstruct(E) defstruct(F) defstruct(G) defstruct(H)
defstruct(I) defstruct(J) defstruct(K) defstruct(L) defstruct(M) defstruct(N) defstruct(O) defstruct(P)
defstruct(Q) defstruct(R) defstruct(S) defstruct(T) defstruct(U) defstruct(V) defstruct(W) defstruct(X)
defstruct(Y) defstruct(Z)

template<typename...> struct FooUnionimpl;

template<typename...> struct typelist;

template<std::size_t V, typename,typename> struct indeximpl;

template<typename T, typename... Ts> struct index {
	enum {value=indeximpl<0,T,typelist<Ts...>>::value };
};

template<std::size_t V, typename T, typename... Ts>
struct indeximpl<V,T,typelist<T,Ts...>> {
	enum { value=V };
};
template<std::size_t V, typename T, typename R, typename... Ts>
struct indeximpl<V,T,typelist<R,Ts...>> {
	enum { value=indeximpl<V+1,T,typelist<Ts...>>::value };
};
template<std::size_t V, typename T>
struct indeximpl<V,T,typelist<>> {
	enum { value=-1 };
};

template<typename FROM, typename TO> struct reindeximpl;

template<typename F1, typename... Fs, typename... Ts>
struct reindeximpl<typelist<F1,Fs...>,typelist<Ts...>> {
	template<std::size_t I>
	constexpr std::size_t exec(std::size_t findex) const {
		switch(findex) {
			case I: return index<F1,Ts...>::value;
			default: return reindeximpl<typelist<Fs...>,typelist<Ts...>>{}.template exec<I+1>(findex);
		}
	}
};
template<typename... Ts>
struct reindeximpl<typelist<>,typelist<Ts...>> {
	template<std::size_t I>
	constexpr std::size_t exec(std::size_t findex) const {
		return -1;
	}
};

template<typename L1, typename L2>
constexpr std::size_t reindex(std::size_t findex) {
	return reindeximpl<L1,L2>{}.template exec<0>(findex);
}

template<std::size_t N>
struct largeenoughint {
	typedef typename std::conditional<
		N<(1L<<8), uint_least8_t,
		typename std::conditional<
			N<(1L<<16), uint_least16_t,
		typename std::conditional<
			N<(1L<<32), uint_least32_t, uint_least64_t>::type
			>::type
			>::type type;
};

template<typename... Ts> struct argstoindextype {
	typedef typename largeenoughint<sizeof...(Ts)>::type type;
};


template<typename RetT, typename... Ts>
constexpr RetT buildunion(bool first, Ts &&...args) {
	if (first) return {std::true_type{},std::forward<Ts>(args)...};
	else return {std::false_type(),std::forward<Ts>(args)...};
}

template<typename T>
struct FooUnionimpl<T> {
	T t;
	using itype = typename argstoindextype<T>::type;
	int foo(itype) { return t.foo(); }
	template<typename TT>
	constexpr FooUnionimpl(TT &&tt) : t(std::forward<TT>(tt)) {}
	template<typename TT>
	constexpr FooUnionimpl(itype,TT &&tt) : t(std::forward<TT>(tt).template get<T>()) {}

	template<typename R, typename std::enable_if<std::is_same<T,R>::value>::type *En=nullptr>
	constexpr R get() const { return t; }
	template<typename R, typename std::enable_if<!std::is_same<T,R>::value>::type *En=nullptr>
	constexpr R get() const { throw std::logic_error("invalid type conversion"); }
};

template<typename T1, typename... Ts>
struct FooUnionimpl<T1,Ts...> {
	union {
		T1 t1;
		FooUnionimpl<Ts...> t2;
	};
	using itype = typename argstoindextype<T1,Ts...>::type;
	int foo(itype i) { return i==0 ? t1.foo() : t2.foo(i-1); }
	constexpr FooUnionimpl(const T1 &t) : t1(t) {}
	template<typename T, typename std::enable_if<!std::is_same<typename std::decay<T>::type,T1>::value>::type *En=nullptr>
	constexpr FooUnionimpl(T &&t): t2(std::forward<T>(t)) {}

	
	template<typename T, typename std::enable_if<std::is_same<typename std::decay<T>::type,T1>::value>::type *En=nullptr>
	constexpr FooUnionimpl(std::true_type, itype i, T &&t) : t1(std::forward<T>(t)) {}
	template<typename T, typename std::enable_if<!std::is_same<typename std::decay<T>::type,T1>::value>::type *En=nullptr>
	constexpr FooUnionimpl(std::true_type, itype i, T &&t) : t1(std::forward<T>(t).template get<T1>()) {}
	template<typename T>
	constexpr FooUnionimpl(std::false_type, itype i, T &&t) : t2(i-1,std::forward<T>(t)) {}
	template<typename T>
	constexpr FooUnionimpl(itype i, T &&t) : FooUnionimpl(buildunion<FooUnionimpl>(i==0,i,std::forward<T>(t))) {}

	template<typename T, typename std::enable_if<std::is_same<T,T1>::value>::type *En=nullptr>
	constexpr T get() const { return t1; }
	template<typename T, typename std::enable_if<!std::is_same<T,T1>::value>::type *En=nullptr>
	constexpr T get() const { return t2.template get<T>(); }
	
};

template<typename...> struct inlst;

template<typename T>
struct inlst<T> { enum {value=0}; };

template<typename T, typename... Ts>
struct inlst<T,T,Ts...> { enum {value=1}; };

template<typename T, typename R, typename... Ts>
struct inlst<T,R,Ts...> { enum {value=inlst<T,Ts...>::value}; };

template<typename... Ts>
struct FooUnion {
	FooUnionimpl<Ts...> x;
	typename FooUnionimpl<Ts...>::itype i;

	int foo() { return x.foo(i); }
	template<typename T, typename std::enable_if<inlst<typename std::decay<T>::type,Ts...>::value>::type *En=nullptr>
	constexpr FooUnion(T &&t) : x(std::forward<T>(t)), i(index<T,Ts...>::value) {}

	template<typename... Ss>
	constexpr FooUnion(const FooUnion<Ss...> &fu) : x(reindex<typelist<Ss...>,typelist<Ts...>>(fu.i),fu.x),
									i(reindex<typelist<Ss...>,typelist<Ts...>>(fu.i)) {}
};

#include "commonunion.h"
COMMONUNION(FooUnionOrig,,foo)

template<typename... Ts>
//using FooU = FooUnionOrig<Ts...>; // 12 seconds
using FooU = FooUnion<Ts...>; // 0.4 seconds

int main(int argc, char **argv) {
/*
	FooUnion<A,B> fu{B{}};
	fu.foo();

	FooUnion<A,B,C> fu2{fu};
	fu2.foo();
*/
/* was 0.28 seconds to compile
 * after adding copy constructor and get, moved to 0.37 seconds */
	FooU<A,C,D,E,F,G,H,I,J,K,L,M,N,O,B> fu{B{}};
	fu.foo();

	FooU<A,B,C,D,E,F,G,H,I,J,Z,K,L,M,N,O> fu2{fu};
	fu2.foo();
/*
 */
}
	
