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
#include "macroforeach.h"

// TODO: add except conditions on all methods & functions

#define CUEXPRSELECT(uname,cond,restrue,resfalse) \
     ((cond) ? uname<typename std::decay<decltype(restrue)>::type, \
                              typename std::decay<decltype(resfalse)>::type>{restrue} \
          : uname<typename std::decay<decltype(restrue)>::type, \
                              typename std::decay<decltype(resfalse)>::type>{resfalse})

namespace commonunion {

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
		//typedef std::size_t type;
     };

     template<typename... Ts> struct argstoindextype {
          typedef typename largeenoughint<sizeof...(Ts)>::type type;
     };

	template<typename...> struct istrivial;

	template<typename T>
	struct istrivial<T> {
		enum {value = std::is_trivially_destructible<T>::value 
				&& std::is_trivially_copy_constructible<T>::value };
	};

	template<typename T1, typename T2, typename... Ts>
	struct istrivial<T1,T2,Ts...> {
		enum {value = istrivial<T1>::value && istrivial<T2,Ts...>::value };
	};

	template<typename T>
	using decay = typename std::decay<T>::type;

	template<template<typename...> class, typename>
	struct istemplateinst {
		enum {value=0};
	};

	template<template<typename...> class H, typename... Ts>
	struct istemplateinst<H,H<Ts...>> {
		enum {value=1};
	};

	template<template<typename...> class, typename>
	struct istemplatebase {
		enum {value=0};
	};

	template<template<typename...> class H, template<typename...> class H2, typename... Ts>
	struct istemplatebase<H,H2<Ts...>> {
		enum {value=std::is_base_of<H<Ts...>,H2<Ts...>>::value};
	};

	//------------------
	
	template<typename RetT, typename... Ts>
	constexpr RetT condbuild(bool usesecond, Ts &&...arg) {
		if (usesecond) return {std::true_type{},std::forward<Ts>(arg)...};
		else return {std::false_type{},std::forward<Ts>(arg)...};
	}

	// now designed for second member to be a cu_node (see below)
	template<typename T1, typename T2>
	struct trivunion {
		union {
			T1 t1;
			T2 t2;
		};
		template<typename... Ts>
		constexpr trivunion(std::true_type, Ts &&...args) : t2(std::forward<Ts>(args)...) {}
		template<typename... Ts>
		constexpr trivunion(std::false_type, Ts &&...args) : t1(std::forward<Ts>(args)...) {}
		template<typename... Ts>
		constexpr trivunion(bool usesecond, Ts &&...args)
			: trivunion(condbuild<trivunion<T1,T2>>(usesecond,std::forward<Ts>(args)...)) {}

		template<typename X>
		void assign(bool wassecond, bool tosecond, X &&x) {
			if (tosecond) t2.assign(std::forward<X>(x));
			else t1.assign(std::forward<X>(x));
		}
	};

	struct dummyT {};

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

		template<typename X>
		void assign(bool wassecond, bool tosecond, X &&x) {
			if (tosecond) {
				if (wassecond) t2.assign(std::forward<X>(x));
				else {
					t1.~T1();
					new (&t2) T2(std::forward<X>(x));
				}
			} else {
				if (!wassecond) t1.assign(std::forward<X>(x));
				else {
					t2.~T2();
					new (&t1) T1(std::forward<X>(x));
				}
			}
		}
	};

	//------------------
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

	//-----------------------

	struct initdirect {};
	

	template<typename...> struct cu_node_base;

	template<typename T1, typename T2, typename... Ts>
	struct cu_node_base<T1,T2,Ts...> {
		using type1 = cu_node_base<T1>;
		using type2 = cu_node_base<T2,Ts...>;
		using itype = typename argstoindextype<T1,T2,Ts...>::type;
		enum { alltrivial = istrivial<T1>::value && type2::alltrivial };
		using myunion = typename std::conditional<alltrivial,
				trivunion<type1,type2>,nontrivunion<type1,type2>>::type;

		myunion x;

		template<typename T,
			typename std::enable_if<std::is_same<decay<T>,T1>::value>::type *En=nullptr>
		constexpr cu_node_base(initdirect,T &&t) : x(std::false_type{},initdirect{},std::forward<T>(t)) {}
		template<typename T,
			typename std::enable_if<!std::is_same<decay<T>,T1>::value>::type *En=nullptr>
		constexpr cu_node_base(initdirect,T &&t) : x(std::true_type{},initdirect{},std::forward<T>(t)) {}

		template<typename... Ss>
		constexpr cu_node_base(itype place, Ss &&...arg)
			: x(place==0,place-1,std::forward<Ss>(arg)...) {}

		template<typename... Ss>
		constexpr cu_node_base(itype fromplace, itype toplace, Ss &&...arg)
					: cu_node_base(toplace==0,fromplace-1,toplace-1,std::forward<Ss>(arg)...) {}

		template<typename V>
		void assign(itype fromplace, itype toplace, V &&v) {
			x.assign(fromplace==0,toplace==0,
			  fromplace-1,toplace-1,std::forward<V>(v));
		}

		template<typename T,
			typename std::enable_if<std::is_same<decay<T>,T1>::value>::type *En=nullptr>
		constexpr const T &get() const { return x.t1.template get<T>(); }
		template<typename T,
			typename std::enable_if<std::is_same<decay<T>,T1>::value>::type *En=nullptr>
		T &get() & { return x.t1.template get<T>(); }
		template<typename T,
			typename std::enable_if<std::is_same<decay<T>,T1>::value>::type *En=nullptr>
		T &&get() && { return std::move(x.t1.template get<T>()); }

		template<typename T,
			typename std::enable_if<!std::is_same<decay<T>,T1>::value>::type *En=nullptr>
		constexpr const T &get() const { return x.t2.template get<T>(); }
		template<typename T,
			typename std::enable_if<!std::is_same<decay<T>,T1>::value>::type *En=nullptr>
		T &get() & { return x.t2.template get<T>(); }
		template<typename T,
			typename std::enable_if<!std::is_same<decay<T>,T1>::value>::type *En=nullptr>
		T &&get() && { return std::move(x.t2.template get<T>()); }

		constexpr void predelete(itype) {}

		template<std::size_t I>
		constexpr auto foo(int a, int b) const ;

		template<std::size_t I, typename F, typename... Ss>
		constexpr auto callfn(itype i, F &&f, Ss &&...args) const {
			switch(i) {
				case I: return x.t1.template callfn<I+1>
							(i,std::forward<F>(f),std::forward<Ss>(args)...);
				default: return x.t2.template callfn<I+1>
							(i,std::forward<F>(f),std::forward<Ss>(args)...);
			}
		}
		template<std::size_t I, typename F, typename... Ss>
		constexpr auto callfn(itype i, F &&f, Ss &&...args) {
			switch(i) {
				case I: return x.t1.template callfn<I+1>
							(i,std::forward<F>(f),std::forward<Ss>(args)...);
				default: return x.t2.template callfn<I+1>
							(i,std::forward<F>(f),std::forward<Ss>(args)...);
			}
		}
		template<std::size_t I, template<typename...> class R, typename F, typename... Ss>
		constexpr auto callfnunion(itype i, F &&f, Ss &&...args) const {
			return CUEXPRSELECT(R,
				(i==I),
					(x.t1.template callfnunion<I+1,R>(i,
						std::forward<F>(f),
						std::forward<Ss>(args)...)),
					(x.t2.template callfnunion<I+1,R>(i,
						std::forward<F>(f),
						std::forward<Ss>(args)...)));
		}
		template<std::size_t I, template<typename...> class R, typename F, typename... Ss>
		constexpr auto callfnunion(itype i, F &&f, Ss &&...args) {
			return CUEXPRSELECT(R,
				(i==I),
					(x.t1.template callfnunion<I+1,R>(i,
						std::forward<F>(f),
						std::forward<Ss>(args)...)),
					(x.t2.template callfnunion<I+1,R>(i,
						std::forward<F>(f),
						std::forward<Ss>(args)...)));
		}
	};

	template<typename T1>
	struct cu_node_base<T1> {
		using itype = typename argstoindextype<T1>::type;
		template<typename... Ss>
		using mybase = cu_node_base<Ss...>;
		enum { alltrivial = istrivial<T1>::value };

		template<typename... Ts>
		constexpr cu_node_base(initdirect,Ts &&...arg) : t1(std::forward<Ts>(arg)...) {}

		template<typename... Ts>
		constexpr cu_node_base(itype, Ts &&...arg) : t1(std::forward<Ts>(arg)...) {}

		template<typename... Ts>
		constexpr cu_node_base(itype fromplace, itype toplace, Ts &&...arg)
				: cu_node_base(toplace,std::forward<Ts>(arg)...) {}

		template<typename... Ss>
		constexpr cu_node_base(const cu_node_base<Ss...> &c) : t1(c.template get<T1>()) {}

		template<typename... Ss>
		constexpr cu_node_base(cu_node_base<Ss...> &&c) : t1(std::move(c).template get<T1>()) {}

		template<typename... Ss>
		constexpr cu_node_base(itype place, const cu_node_base<Ss...> &c) : t1(c.template get<T1>()) {}

		template<typename... Ss>
		constexpr cu_node_base(itype place, cu_node_base<Ss...> &&c) : t1(std::move(c).template get<T1>()) {}

		template<typename V,
			typename std::enable_if<!istemplateinst<mybase,decay<V>>::value>::type *En=nullptr>
		void assign(itype fromplace, itype toplace, V &&v) {
			t1 = v;
		}

		template<typename... Ss>
		void assign(itype fromplace, itype toplace, const cu_node_base<Ss...> &c) {
			t1 = c.template get<T1>();
		}

		template<typename... Ss>
		void assign(itype fromplace, itype toplace, cu_node_base<Ss...> &&c) {
			t1 = std::move(c).template get<T1>();
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
		template<typename T,
			typename std::enable_if<!std::is_same<T,T1>::value>::type *En=nullptr>
		constexpr T &get() & { throw std::logic_error("invalid type conversion"); }
		template<typename T,
			typename std::enable_if<!std::is_same<T,T1>::value>::type *En=nullptr>
		constexpr T &&get() && { throw std::logic_error("invalid type conversion"); }

		template<std::size_t, typename F, typename... Ss>
		constexpr auto callfn(itype i, F &&f, Ss &&...args) const {
			return f(t1,std::forward<Ss>(args)...);
		}
		template<std::size_t, typename F, typename... Ss>
		constexpr auto callfn(itype i, F &&f, Ss &&...args) {
			return f(t1,std::forward<Ss>(args)...);
		}
		template<std::size_t, template<typename...> class, typename F, typename... Ss>
		constexpr auto callfnunion(itype i, F &&f, Ss &&...args) const {
			return f(t1,std::forward<Ss>(args)...);
		}
		template<std::size_t, template<typename...> class, typename F, typename... Ss>
		constexpr auto callfnunion(itype i, F &&f, Ss &&...args) {
			return f(t1,std::forward<Ss>(args)...);
		}

		constexpr void predelete(itype) {}
		T1 t1;
	};

	//-------------------------------

	template<typename...> struct cu_node_nontriv;

	template<typename T1, typename T2, typename... Ts>
	struct cu_node_nontriv<T1,T2,Ts...> : cu_node_base<T1,T2,Ts...> {
		using B = cu_node_base<T1,T2,Ts...>;
		using B::B;
		using itype = typename B::itype;
		using type1 = typename B::type1;
		using type2 = typename B::type2;

		constexpr void predelete(itype i) {
			switch(lastbit(i)) {
				case 0: this->x.t1.predelete(restbits(i)); break;
				default: this->x.t2.predelete(restbits(i)); break;
			}
			this->x.predelete(lastbit(i));
		}
	};

	template<typename T1>
	struct cu_node_nontriv<T1> : cu_node_base<T1> {
		using B = cu_node_base<T1>;
		using itype = typename B::itype;
		using B::B;
	};

	//-------------------------------

	template<typename...Ts>
	using cu_node = typename std::conditional<
			istrivial<Ts...>::value,
				cu_node_base<Ts...>,
				cu_node_nontriv<Ts...>>::type;

	//-------------------------------

	template<typename... Ts>
	struct cu_impl_base { 
		using baseT = cu_node<Ts...>;
		baseT v;
		typename baseT::itype i;
		template<typename... Ss>
		using mybase = cu_impl_base<Ss...>;

		template<typename T,
			typename std::enable_if<!istemplatebase<mybase,decay<T>>::value>::type *En=nullptr>
		explicit constexpr cu_impl_base(T &&t) : v(initdirect{},std::forward<T>(t)),
				i(index<decay<T>,Ts...>::value) {}

		explicit constexpr cu_impl_base(const cu_impl_base &c) : v(c.i,c.v), i(c.i) {}
		explicit constexpr cu_impl_base(cu_impl_base &&c) : v(c.i,std::move(c.v)), i(c.i) {}

		template<typename... Ss>
		explicit constexpr cu_impl_base(const cu_impl_base<Ss...> &c)
				: v(reindex<typelist<Ss...>,typelist<Ts...>>(c.i),c.v),
				  i(reindex<typelist<Ss...>,typelist<Ts...>>(c.i)) {}

		template<typename... Ss>
		explicit constexpr cu_impl_base(cu_impl_base<Ss...> &&c)
				: v(reindex<typelist<Ss...>,typelist<Ts...>>(c.i),std::move(c.v)),
				  i(reindex<typelist<Ss...>,typelist<Ts...>>(c.i)) {}

		template<typename T,
			typename std::enable_if<!istemplatebase<mybase,decay<T>>::value>::type *En=nullptr>
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
		template<typename... Ss>
		cu_impl_base &operator=(const cu_impl_base<Ss...> &c) {
			auto oldi = i;
			i = reindex<typelist<Ss...>,typelist<Ts...>>(c.i);
			v.assign(oldi,i,c.v);
			return *this;
		}
		template<typename... Ss>
		cu_impl_base &operator=(cu_impl_base<Ss...> &&c) {
			auto oldi = i;
			i = reindex<typelist<Ss...>,typelist<Ts...>>(c.i);
			v.assign(oldi,i,std::move(c.v));
			return *this;
		}

		template<typename F, typename... Ss>
		constexpr auto callfn(F &&f, Ss &&...args) const {
			return v.template callfn<0>(i,std::forward<F>(f),std::forward<Ss>(args)...);
		}
		template<typename F, typename... Ss>
		constexpr auto callfn(F &&f, Ss &&...args) {
			return v.template callfn<0>(i,std::forward<F>(f),std::forward<Ss>(args)...);
		}

		template<template<typename...> class R, typename F, typename... Ss>
		constexpr auto callfnunion(F &&f, Ss &&...args) const {
			return v.template callfnunion<0,R>(i,std::forward<F>(f),std::forward<Ss>(args)...);
		}

		template<template<typename...> class R, typename F, typename... Ss>
		constexpr auto callfnunion(F &&f, Ss &&...args) {
			return v.template callfnunion<0,R>(i,std::forward<F>(f),std::forward<Ss>(args)...);
		}
	};

	template<typename... Ts>
	struct cu_impl_nontriv : public cu_impl_base<Ts...> {
		using B = cu_impl_base<Ts...>;
		using B::B;

		using B::operator=;

		~cu_impl_nontriv() { this->v.predelete(this->i); }
	};

	//--------------------------
	template<typename...Ts>
	using cu_impl = typename std::conditional<
			istrivial<Ts...>::value,
				cu_impl_base<Ts...>,
				cu_impl_nontriv<Ts...>>::type;

}


// Note:: cannot use lambda-expression below, as they are not allowed in
// constexpr yet
#define WRITEDISPATCHN1(fname) \
	struct exec_##fname { \
		template<typename T, typename... Args> \
		constexpr auto operator()(T &&t, Args &&...args) const { \
			return (std::forward<T>(t)).fname(std::forward<Args>(args)...); \
		} \
	}; \
	template<typename... Ss> \
	constexpr auto fname(Ss &&...args) const \
		{ return this->callfn(exec_##fname{},std::forward<Ss>(args)...);} \
	template<typename... Ss> \
	constexpr auto fname(Ss &&...args) \
		{ return this->callfn(exec_##fname{},std::forward<Ss>(args)...);} \

#define WRITEDISPATCHN2(rettype,fname) \
	struct exec_##fname { \
		template<typename T, typename... Args> \
		constexpr auto operator()(T &&t, Args &&...args) const { \
			return (std::forward<T>(t)).fname(std::forward<Args>(args)...); \
		} \
	}; \
	template<typename... Ss> \
	constexpr auto fname(Ss &&...args) const \
		{ return this->callfnunion<rettype>(exec_##fname{},std::forward<Ss>(args)...);} \
	template<typename... Ss> \
	constexpr auto fname(Ss &&...args) \
		{ return this->callfnunion<rettype>(exec_##fname{},std::forward<Ss>(args)...);} \

#define WRITEDISPATCH(ARG) CALLVAR_N2(WRITEDISPATCHN,STRIPPAREN(ARG))

#define IGNOREARG(...)

#define ARGCOMMA(x) x,

#define COMMONUNION(cname,baseclause,...) \
template<typename... Ts> \
struct cname##_impl : IFEMPTY(IGNOREARG,ARGCOMMA,baseclause,baseclause) \
				public commonunion::cu_impl<Ts...> { \
	using commonunion::cu_impl<Ts...>::cu_impl; \
 \
	FOREACH(WRITEDISPATCH,__VA_ARGS__) \
}; \
 \
template<typename... Ts> \
using cname = \
	typename splittype::stripdupargs< \
		typename splittype::flattentype< \
			cname##_impl<>,Ts...>::type \
          >::type;

#endif
