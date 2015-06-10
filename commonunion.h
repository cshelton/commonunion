#ifndef COMMONUNION_H
#define COMMONUNION_H

/*The MIT License (MIT)
 *
 * Copyright (c) 2015 Christian Shelton
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include <utility>
#include <type_traits>
#include <stdexcept>

#include "splittype.h"

// TODO: add except conditions on all methods & functions
// TODO: put in its own namespace
// TODO: add MACROS (from old version)
// TODO: add variable-sized itype

template<typename...> struct alltrivialcopy {
	enum { value=1 };
};

template<typename T1, typename... Ts>
struct alltrivialcopy<T1,Ts...> {
	enum {value = (std::is_void<T1>::value || std::is_trivially_copyable<T1>::value)
				&& alltrivialcopy<Ts...>::value};
};

template<typename T>
using decay = typename std::decay<T>::type;

//------------------
//
template<typename RetT, typename... Ts>
constexpr RetT condbuild(bool usesecond, Ts &&...arg) {
	if (usesecond) return {std::true_type{},std::forward<Ts>(arg)...};
	else return {std::false_type{},std::forward<Ts>(arg)...};
}

struct dummyT {};

template<typename T1, typename T2>
struct trivunion {
	union {
		dummyT dummy;
		T1 t1;
		T2 t2;
	};
	constexpr trivunion(): dummy() {}
	template<typename... Ts>
	constexpr trivunion(std::true_type, Ts &&...args) : t2(std::forward<Ts>(args)...) {}
	template<typename... Ts>
	constexpr trivunion(std::false_type, Ts &&...args) : t1(std::forward<Ts>(args)...) {}
	template<typename... Ts>
	constexpr trivunion(bool usesecond, Ts &&...args)
		: trivunion(condbuild<trivunion>(usesecond,std::forward<Ts>(args)...)) {}

	template<typename... X>
	void assign(bool wassecond, bool tosecond, X &&...x) {
		if (tosecond) t2.assign(std::forward<X>(x)...);
		else t1.assign(std::forward<X>(x)...);
	}
};

template<typename T1, typename T2>
struct nontrivunion {
	union {
		dummyT dummy;
		T1 t1;
		T2 t2;
	};
	constexpr nontrivunion(): dummy() {}
	template<typename... Ts>
	constexpr nontrivunion(std::true_type, Ts &&...args) : t2(std::forward<Ts>(args)...) {}
	template<typename... Ts>
	constexpr nontrivunion(std::false_type, Ts &&...args) : t1(std::forward<Ts>(args)...) {}
	template<typename... Ts>
	constexpr nontrivunion(bool usesecond, Ts &&...args) : dummy() {
		if (usesecond) new (&t2) T2(std::forward<Ts>(args)...);
		else new (&t1) T1(std::forward<Ts>(args)...);
	}

	~nontrivunion() {}

	void predelete(bool delsecond) {
		if (delsecond) t2.~T2();
		else t1.~T1();
	}

	template<typename... X>
	void assign(bool wassecond, bool tosecond, X &&...x) {
		if (tosecond) {
			if (wassecond) t2.assign(std::forward<X>(x)...);
			else {
				t1.~T1();
				new (&t2) T2(std::forward<X>(x)...);
			}
		} else {
			if (!wassecond) t1.assign(std::forward<X>(x)...);
			else {
				t2.~T2();
				new (&t1) T1(std::forward<X>(x)...);
			}
		}
	}
};


template<typename T1, typename T2>
using delunion = typename std::conditional<
		std::is_trivially_destructible<T1>::value && std::is_trivially_destructible<T2>::value,
			trivunion<T1,T2>,
			nontrivunion<T1,T2>>::type;


//------------------

struct gencunode {
};

template<template<typename...> class, typename...> struct cu_node_base;



template<template<typename...> class R, typename T1, typename T2, typename... Ts>
struct cu_node_base<R,T1,T2,Ts...> : public gencunode {
	using type1 = typename splitargtype_odds<R<T1,T2,Ts...>>::type;
	using type2 = typename splitargtype_evens<R<T1,T2,Ts...>>::type;

	using itype = std::size_t; // TODO

	template<typename T, typename EN=void>
	struct index { enum {value=0}; };
	template<typename T>
	struct index<T,typename std::enable_if<
					isargtype<decay<T>,type1>::value>::type> {
		enum {value=type1::template index<T>::value*2}; };
	template<typename T>
	struct index<T,typename std::enable_if<
					isargtype<decay<T>,type2>::value>::type> {
		enum {value=1+type2::template index<T>::value*2}; };

	constexpr cu_node_base(dummyT) : x() {};

	template<typename T,
		typename std::enable_if<isargtype<decay<T>,type1>::value>::type *En=nullptr>
	constexpr cu_node_base(T &&t) : x(std::false_type{},std::forward<T>(t)) {}
	template<typename T,
		typename std::enable_if<isargtype<decay<T>,type2>::value>::type *En=nullptr>
	constexpr cu_node_base(T &&t) : x(std::true_type{},std::forward<T>(t)) {}

	template<typename... Ss>
	constexpr cu_node_base(itype place, Ss &&...arg) : x(place&1,place>>1,std::forward<Ss>(arg)...) {}

	template<typename... Ss>
	constexpr cu_node_base(itype fromplace, itype toplace, Ss &&...arg)
				: cu_node_base(toplace,std::forward<Ss>(arg)...) {}

	template<typename... V>
	void assign(itype fromplace, itype toplace, V &&...v) {
		x.assign(fromplace&1,toplace&1,fromplace>>1,toplace>>1,std::forward<V>(v)...);
	}

	template<template<typename...> class R2, typename... Ss>
	static constexpr typename cu_node_base<R2,Ss...>::itype reindex(itype i) {
		if (i&1) return type2::template reindex<R2,Ss...>(i>>1);
		else return type1::template reindex<R2,Ss...>(i>>1);
	}

	template<typename T,
		typename std::enable_if<isargtype<decay<T>,type1>::value>::type *En=nullptr>
	constexpr const T &get() const { return x.t1.template get<T>(); }
	template<typename T,
		typename std::enable_if<!isargtype<decay<T>,type1>::value>::type *En=nullptr>
	constexpr const T &get() const { return x.t2.template get<T>(); }
	template<typename T,
		typename std::enable_if<isargtype<decay<T>,type1>::value>::type *En=nullptr>
	T &get() & { return x.t1.template get<T>(); }
	template<typename T,
		typename std::enable_if<!isargtype<decay<T>,type1>::value>::type *En=nullptr>
	T &get() & { return x.t2.template get<T>(); }
	template<typename T,
		typename std::enable_if<isargtype<decay<T>,type1>::value>::type *En=nullptr>
	T &&get() && { return std::move(x.t1.template get<T>()); }
	template<typename T,
		typename std::enable_if<!isargtype<decay<T>,type1>::value>::type *En=nullptr>
	T &get() && { return std::move(x.t2.template get<T>()); }

	constexpr void predelete(itype) {}

	delunion<type1,type2> x;
};


template<template<typename...> class R, typename T1>
struct cu_node_base<R,T1> : public gencunode {
	using itype = std::size_t; // TODO

	template<typename T, typename EN=void>
	struct index { enum {value=0}; };

	template<typename... Ts>
	constexpr cu_node_base(Ts &&...arg) : t1(std::forward<Ts>(arg)...) {}

	template<typename... Ts>
	constexpr cu_node_base(itype, Ts &&...arg) : t1(std::forward<Ts>(arg)...) {}

	template<typename... Ts>
	constexpr cu_node_base(itype fromplace, itype toplace, Ts &&...arg)
			: cu_node_base(toplace,std::forward<Ts>(arg)...) {}

	template<template<typename...> class R2, typename... Ss>
	constexpr cu_node_base(const cu_node_base<R2,Ss...> &c) : t1(c.template get<T1>()) {}

	template<template<typename...> class R2, typename... Ss>
	constexpr cu_node_base(cu_node_base<R2,Ss...> &&c) : t1(std::move(c).template get<T1>()) {}

	template<template<typename...> class R2, typename... Ss>
	constexpr cu_node_base(itype place, const cu_node_base<R2,Ss...> &c) : t1(c.template get<T1>()) {}

	template<template<typename...> class R2, typename... Ss>
	constexpr cu_node_base(itype place, cu_node_base<R2,Ss...> &&c) : t1(std::move(c).template get<T1>()) {}

	template<typename... Ss>
	constexpr cu_node_base(itype place, const R<Ss...> &c) : t1(c.template get<T1>()) {}

	template<typename... Ss>
	constexpr cu_node_base(itype place, R<Ss...> &&c) : t1(std::move(c).template get<T1>()) {}


	template<typename V, typename std::enable_if<!std::is_base_of<gencunode,decay<V>>::value>::type *En=nullptr>
	void assign(itype fromplace, itype toplace, V &&v) {
		t1 = v;
	}

	template<template<typename...> class R2, typename... Ss>
	void assign(itype fromplace, itype toplace, const cu_node_base<R2,Ss...> &c) {
		t1 = c.template get<T1>();
	}

	template<template<typename...> class R2, typename... Ss>
	void assign(itype fromplace, itype toplace, cu_node_base<R2,Ss...> &&c) {
		t1 = std::move(c).template get<T1>();
	}
		

	template<template<typename...> class R2, typename... Ss>
	static constexpr typename cu_node_base<R2,Ss...>::itype reindex(itype i) {
		return cu_node_base<R2,Ss...>::template index<T1>::value;
	}

	template<typename T,
		typename std::enable_if<std::is_same<T,T1>::value>::type *En=nullptr>
	constexpr const T1 &get() const { return t1; }
	template<typename T,
		typename std::enable_if<std::is_same<T,T1>::value>::type *En=nullptr>
	T1 &get() & { return t1; }
	template<typename T,
		typename std::enable_if<std::is_same<T,T1>::value>::type *En=nullptr>
	T1 &&get() && { return std::move(t1); }

	template<typename T,
		typename std::enable_if<!std::is_same<T,T1>::value>::type *En=nullptr>
	constexpr const T &get() const { throw std::logic_error("invalid type conversion"); }

	constexpr void predelete(itype) {}
	T1 t1;
};
//-------------------------------

template<template<typename...> class, typename...> struct cu_node_nontriv;

template<template<typename...> class R, typename T1, typename T2, typename... Ts>
struct cu_node_nontriv<R,T1,T2,Ts...> : cu_node_base<R,T1,T2,Ts...> {
	using B = cu_node_base<R,T1,T2,Ts...>;
	using B::B;
	using itype = typename B::itype;
	using type1 = typename B::type1;
	using type2 = typename B::type2;

	constexpr void predelete(itype i) {
		if (i&1) this->x.t2.predelete(i>>1);
		else this->x.t1.predelete(i>>1);
		this->x.predelete(i&1);
	}

};

template<template<typename...> class R, typename T1>
struct cu_node_nontriv<R,T1> : cu_node_base<R,T1> {
	using B = cu_node_base<R,T1>;
	using itype = typename B::itype;
	using B::B;

};

//-------------------------------

template<template<typename...> class R, typename...Ts>
using cu_node = typename std::conditional<
		alltrivialcopy<Ts...>::value,
			cu_node_base<R,Ts...>,
			cu_node_nontriv<R,Ts...>>::type;

//-------------------------------
struct gencu {};

template<template<typename...> class R, typename... Ts>
struct cu_impl_base : public gencu {
	using baseT = R<Ts...>;
	baseT v;
	typename baseT::itype i;

	template<typename T, typename std::enable_if<!std::is_base_of<gencu,decay<T>>::value>::type *En=nullptr>
	explicit constexpr cu_impl_base(T &&t) : v(std::forward<T>(t)),
			i(baseT::template index<decay<T>>::value) {}

	explicit constexpr cu_impl_base(const cu_impl_base &c) : v(c.i,c.v), i(c.i) {}
	explicit constexpr cu_impl_base(cu_impl_base &&c) : v(c.i,std::move(c.v)), i(c.i) {}

	template<template<typename...> class R2, typename... Ss>
	explicit constexpr cu_impl_base(const cu_impl_base<R2,Ss...> &c)
			: v(cu_impl_base<R2,Ss...>::baseT::template reindex<R,Ts...>(c.i),c.v),
			  i(cu_impl_base<R2,Ss...>::baseT::template reindex<R,Ts...>(c.i)) {}

	template<template<typename...> class R2, typename... Ss>
	explicit constexpr cu_impl_base(cu_impl_base<R2,Ss...> &&c)
			: v(cu_impl_base<R2,Ss...>::baseT::template reindex<R,Ts...>(c.i),std::move(c.v)),
			  i(cu_impl_base<R2,Ss...>::baseT::template reindex<R,Ts...>(c.i)) {}

	template<typename T, typename std::enable_if<!std::is_base_of<gencu,decay<T>>::value>::type *En=nullptr>
	cu_impl_base &operator=(T &&t) {
		auto oldi = i;
		i = baseT::template index<decay<T>>::value;
		v.assign(oldi,i,std::forward<T>(t));
		return *this;
	}

	cu_impl_base &operator=(const cu_impl_base &c) {
		auto oldi = i;
		i = c.i;
		v.assign(oldi,i,c.v);
		return *this;
	}
	cu_impl_base &operator=(cu_impl_base &&c) {
		auto oldi = i;
		i = c.i;
		v.assign(oldi,i,std::move(c.v));
		return *this;
	}
	template<template<typename...> class R2, typename... Ss>
	cu_impl_base &operator=(const cu_impl_base<R2,Ss...> &c) {
		auto oldi = i;
		i = cu_impl_base<R2,Ss...>::baseT::template reindex<R,Ts...>(c.i);
		v.assign(oldi,i,c.v);
		return *this;
	}
	template<template<typename...> class R2, typename... Ss>
	cu_impl_base &operator=(cu_impl_base<R2,Ss...> &&c) {
		auto oldi = i;
		i = cu_impl_base<R2,Ss...>::baseT::template reindex<R,Ts...>(c.i);
		v.assign(oldi,i,std::move(c.v));
		return *this;
	}

};

template<template<typename...> class R, typename... Ts>
struct cu_impl_nontriv : public cu_impl_base<R,Ts...> {
	using B = cu_impl_base<R,Ts...>;
	using B::B;

	using B::operator=;

	~cu_impl_nontriv() { this->v.predelete(this->i); }
};

//--------------------------
template<template<typename...> class R, typename...Ts>
using cu_impl = typename std::conditional<
		alltrivialcopy<Ts...>::value,
			cu_impl_base<R,Ts...>,
			cu_impl_nontriv<R,Ts...>>::type;

//-------------------------------

template<typename...> struct foo_cu_node;
template<typename...> struct foo_cu_impl;

template<typename T1, typename T2, typename... Ts>
struct foo_cu_node<T1,T2,Ts...> : public cu_node<foo_cu_node,T1,T2,Ts...> {
	using B = cu_node<::foo_cu_node,T1,T2,Ts...>;
		// :: for clang bug (see http://stackoverflow.com/questions/17687459/
		// 					clang-not-accepting-use-of-template-template-parameter-when-using-crtp)
	using itype = typename B::itype;

	using B::B;
	template<typename... Ss>
	constexpr auto foo(itype i, Ss &&...args) const {
		if (i&1) return this->x.t2.foo(i>>1,std::forward<Ss>(args)...);
		else return this->x.t1.foo(i>>1,std::forward<Ss>(args)...);
	}
	template<typename... Ss>
	constexpr auto foo(itype i, Ss &&...args) {
		if (i&1) return this->x.t2.foo(i>>1,std::forward<Ss>(args)...);
		else return this->x.t1.foo(i>>1,std::forward<Ss>(args)...);
	}
};

template<typename T1>
struct foo_cu_node<T1> : public cu_node<foo_cu_node,T1> {
	using B = cu_node<::foo_cu_node,T1>; // :: for reasons above
	using itype = typename B::itype;

	using B::B;
	template<typename... Ss>
	constexpr auto foo(itype i, Ss &&...args) const { return this->t1.foo(std::forward<Ss>(args)...);}
	template<typename... Ss>
	constexpr auto foo(itype i, Ss &&...args) { return this->t1.foo(std::forward<Ss>(args)...);}
};

template<typename... Ts>
struct foo_cu_impl : public cu_impl<foo_cu_node,Ts...> {
	using B = cu_impl<::foo_cu_node,Ts...>;
	using B::B;
	using B::operator=;
	template<typename... Ss>
	constexpr auto foo(Ss &&...args) const { return this->v.foo(this->i,std::forward<Ss>(args)...);}
	template<typename... Ss>
	constexpr auto foo(Ss &&...args) { return this->v.foo(this->i,std::forward<Ss>(args)...);}
};

#endif
