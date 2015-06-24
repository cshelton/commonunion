#ifndef SPLITTYPE_H
#define SPLITTYPE_H

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

namespace splittype {
	template<typename...> struct splittype_odds;
	template<typename...> struct splittype_evens;

	template<typename... Rs, template<typename...> class H,
		typename T1, typename T2, typename... Ts>
	struct splittype_odds<H<Rs...>,T1,T2,Ts...> {
		typedef typename splittype_odds<H<Rs...,T1>,Ts...>::type type;
	};
	template<typename... Rs, template<typename...> class H,
		typename T1>
	struct splittype_odds<H<Rs...>,T1> {
		typedef H<Rs...,T1> type;
	};
	template<typename... Rs, template<typename...> class H>
	struct splittype_odds<H<Rs...>> {
		typedef H<Rs...> type;
	};

	template<template<typename...> class C, typename... Rs, template<template<typename...> class, typename...> class H,
		typename T1, typename T2, typename... Ts>
	struct splittype_odds<H<C,Rs...>,T1,T2,Ts...> {
		typedef typename splittype_odds<H<C,Rs...,T1>,Ts...>::type type;
	};
	template<template<typename...> class C, typename... Rs, template<template<typename...> class, typename...> class H,
		typename T1>
	struct splittype_odds<H<C,Rs...>,T1> {
		typedef H<C,Rs...,T1> type;
	};
	template<template<typename...> class C, typename... Rs, template<template<typename...> class, typename...> class H>
	struct splittype_odds<H<C,Rs...>> {
		typedef H<C,Rs...> type;
	};

	template<typename... Rs, template<typename...> class H,
		typename T1, typename T2, typename... Ts>
	struct splittype_evens<H<Rs...>,T1,T2,Ts...> {
		typedef typename splittype_evens<H<Rs...,T2>,Ts...>::type type;
	};   
	template<typename... Rs, template<typename...> class H,
		typename T1>
	struct splittype_evens<H<Rs...>,T1> {
		typedef H<Rs...> type;
	};
	template<typename... Rs, template<typename...> class H>
	struct splittype_evens<H<Rs...>> {
		typedef H<Rs...> type;
	};

	template<template<typename...> class C, typename... Rs, template<template<typename...> class, typename...> class H,
		typename T1, typename T2, typename... Ts>
	struct splittype_evens<H<C,Rs...>,T1,T2,Ts...> {
		typedef typename splittype_evens<H<C,Rs...,T2>,Ts...>::type type;
	};   
	template<template<typename...> class C, typename... Rs, template<template<typename...> class, typename...> class H,
		typename T1>
	struct splittype_evens<H<C,Rs...>,T1> {
		typedef H<C,Rs...> type;
	};
	template<template<typename...> class C, typename... Rs, template<template<typename...> class, typename...> class H>
	struct splittype_evens<H<C,Rs...>> {
		typedef H<C,Rs...> type;
	};
	//-----------------------------
	template<typename...> struct splittype0;
	template<typename...> struct splittype1;
	template<typename...> struct splittype2;
	template<typename...> struct splittype3;

	template<typename... Rs, template<typename...> class H, typename T1, typename T2, typename T3, typename T4, typename... Ts>
	struct splittype0<H<Rs...>,T1,T2,T3,T4,Ts...> {
		typedef typename splittype0<H<Rs...,T1>,Ts...>::type type;
	};
	template<typename... Rs, template<typename...> class H, typename T1, typename T2, typename T3, typename T4, typename... Ts>
	struct splittype1<H<Rs...>,T1,T2,T3,T4,Ts...> {
		typedef typename splittype1<H<Rs...,T2>,Ts...>::type type;
	};
	template<typename... Rs, template<typename...> class H, typename T1, typename T2, typename T3, typename T4, typename... Ts>
	struct splittype2<H<Rs...>,T1,T2,T3,T4,Ts...> {
		typedef typename splittype2<H<Rs...,T3>,Ts...>::type type;
	};
	template<typename... Rs, template<typename...> class H, typename T1, typename T2, typename T3, typename T4, typename... Ts>
	struct splittype3<H<Rs...>,T1,T2,T3,T4,Ts...> {
		typedef typename splittype3<H<Rs...,T4>,Ts...>::type type;
	};

	template<typename... Rs, template<typename...> class H, typename T1, typename T2, typename T3>
	struct splittype0<H<Rs...>,T1,T2,T3> {
		typedef H<Rs...,T1> type;
	};
	template<typename... Rs, template<typename...> class H, typename T1, typename T2, typename T3>
	struct splittype1<H<Rs...>,T1,T2,T3> {
		typedef H<Rs...,T2> type;
	};
	template<typename... Rs, template<typename...> class H, typename T1, typename T2, typename T3>
	struct splittype2<H<Rs...>,T1,T2,T3> {
		typedef H<Rs...,T3> type;
	};
	template<typename... Rs, template<typename...> class H, typename T1, typename T2, typename T3>
	struct splittype3<H<Rs...>,T1,T2,T3> {
		typedef H<Rs...> type;
	};

	template<typename... Rs, template<typename...> class H, typename T1, typename T2>
	struct splittype0<H<Rs...>,T1,T2> {
		typedef H<Rs...,T1> type;
	};
	template<typename... Rs, template<typename...> class H, typename T1, typename T2>
	struct splittype1<H<Rs...>,T1,T2> {
		typedef H<Rs...,T2> type;
	};
	template<typename... Rs, template<typename...> class H, typename T1, typename T2>
	struct splittype2<H<Rs...>,T1,T2> {
		typedef H<Rs...> type;
	};
	template<typename... Rs, template<typename...> class H, typename T1, typename T2>
	struct splittype3<H<Rs...>,T1,T2> {
		typedef H<Rs...> type;
	};

	template<typename... Rs, template<typename...> class H, typename T1>
	struct splittype0<H<Rs...>,T1> {
		typedef H<Rs...,T1> type;
	};
	template<typename... Rs, template<typename...> class H, typename T1>
	struct splittype1<H<Rs...>,T1> {
		typedef H<Rs...> type;
	};
	template<typename... Rs, template<typename...> class H, typename T1>
	struct splittype2<H<Rs...>,T1> {
		typedef H<Rs...> type;
	};
	template<typename... Rs, template<typename...> class H, typename T1>
	struct splittype3<H<Rs...>,T1> {
		typedef H<Rs...> type;
	};

	template<typename... Rs, template<typename...> class H>
	struct splittype0<H<Rs...>> {
		typedef H<Rs...> type;
	};
	template<typename... Rs, template<typename...> class H>
	struct splittype1<H<Rs...>> {
		typedef H<Rs...> type;
	};
	template<typename... Rs, template<typename...> class H>
	struct splittype2<H<Rs...>> {
		typedef H<Rs...> type;
	};
	template<typename... Rs, template<typename...> class H>
	struct splittype3<H<Rs...>> {
		typedef H<Rs...> type;
	};

	template<typename...> struct splitargtype0;
	template<typename...> struct splitargtype1;
	template<typename...> struct splitargtype2;
	template<typename...> struct splitargtype3;

	template<typename... Rs, template<typename...> class H>
	struct splitargtype0<H<Rs...>> {
		typedef typename splittype0<H<>,Rs...>::type type;
	};
	template<typename... Rs, template<typename...> class H>
	struct splitargtype1<H<Rs...>> {
		typedef typename splittype1<H<>,Rs...>::type type;
	};
	template<typename... Rs, template<typename...> class H>
	struct splitargtype2<H<Rs...>> {
		typedef typename splittype2<H<>,Rs...>::type type;
	};
	template<typename... Rs, template<typename...> class H>
	struct splitargtype3<H<Rs...>> {
		typedef typename splittype3<H<>,Rs...>::type type;
	};

	//-----------------------------
	template<typename,typename...> struct intypelist;

	template<typename T>
	struct intypelist<T> { enum {value=0}; };

	template<typename T, typename... Ts>
	struct intypelist<T,T,Ts...> { enum {value=1}; };
	
	template<typename T, typename T1, typename... Ts>
	struct intypelist<T,T1,Ts...> { enum {value=intypelist<T,Ts...>::value}; };

	template<typename, typename>
	struct isargtype;

	template<typename R1, typename... Rs, template<typename...> class H>
	struct isargtype<R1,H<Rs...>> { enum { value=intypelist<R1,Rs...>::value}; };
	template<typename R1, typename... Rs, template<typename...> class C, template<template<typename...> class, typename...> class H>
	struct isargtype<R1,H<C,Rs...>> { enum { value=intypelist<R1,Rs...>::value}; };

/*
	template<typename,typename> struct isargtype {
		enum {value=0 };
	};

	// base cases for 1 *and* 2 type args
	// to prevent infinite recursion.

	template<typename R1, typename R2, typename... Rs, template<typename...> class H>
	struct isargtype<R1,H<R1,R2,Rs...>> {
		enum {value=1 };
	};

	template<typename R1, typename R2, typename R3, typename ... Rs, template<typename...> class H>
	struct isargtype<R1,H<R2,R3,Rs...>> {
		enum {value=isargtype<R1,H<R3,Rs...>>::value };
	};

	template<typename R1, template<typename...> class H>
	struct isargtype<R1,H<R1>> {
		enum {value=1 };
	};

	template<typename R1, typename R2, template<typename...> class H>
	struct isargtype<R1,H<R2>> {
		enum {value=0 };
	};
*/

	/*
	template<typename R1, template<typename...> class H>
	struct isargtype<R1,H<>> {
		enum {value=0 };
	};
	*/

	//-----------------------------

	template<typename...> struct strip_dups;

/*
	template<typename... Rs, template<typename...> class H,
			typename T1, typename... Ts>
	struct strip_dups<H<Rs...>,T1,Ts...> {
		typedef typename std::conditional<isargtype<T1,H<Rs...>>::value,
				typename strip_dups<H<Rs...>,Ts...>::type,
				typename strip_dups<H<Rs...,T1>,Ts...>::type>::type type;
	};

	template<typename... Ts, template<typename...> class H,
			typename T1, typename... Ts,
			typename 

	template<typename... Rs, template<typename...> class H>
	struct strip_dups<H<Rs...>> {
		typedef H<Rs...> type;
	};
*/

	template<template<typename...> class H, typename L, typename T,
			bool toadd = !isargtype<T,L>::value> struct setadd;

	template<template<typename...> class H, typename... Ts, typename T>
	struct setadd<H,H<Ts...>,T,true> {
		typedef H<Ts...,T> type;
	};

	template<template<typename...> class H, typename... Ts, typename T>
	struct setadd<H,H<Ts...>,T,false> {
		typedef H<Ts...> type;
	};

	template<template<typename...> class H, typename... Ts>
	struct strip_dups<H<Ts...>> {
		typedef H<Ts...> type;
	};

	template<template<typename...> class H, typename... Ts, typename R, typename... Rs>
	struct strip_dups<H<Ts...>,R,Rs...> {
		typedef typename strip_dups<typename setadd<H,H<Ts...>,R>::type,Rs...>::type type;
	};

	template<typename...> struct stripdupargs;

	template<typename... Rs, template<typename...> class H>
	struct stripdupargs<H<Rs...>> {
		typedef typename strip_dups<H<>,Rs...>::type type;
	};

	//-----------------------------
	
	template<typename Lcheck, typename Ladd, typename T,
			bool toadd = !isargtype<T,Lcheck>::value> struct setadd2;

	template<template<typename...> class H, typename... Ss, typename... Ts, typename T>
	struct setadd2<H<Ss...>,H<Ts...>,T,true> {
		typedef H<Ts...,T> type;
	};
	template<template<typename...> class H, typename... Ss, typename... Ts, typename T>
	struct setadd2<H<Ss...>,H<Ts...>,T,false> {
		typedef H<Ts...> type;
	};

	template<typename...> struct strip_dups2;

	template<template<typename...> class H, typename... Ts, typename... Ss>
	struct strip_dups2<H<Ss...>,H<Ts...>> {
		typedef H<Ts...> type;
	};

	template<template<typename...> class H, typename... Ts, typename R, typename... Rs, typename... Ss>
	struct strip_dups2<H<Ss...>,H<Ts...>,R,Rs...> {
		typedef typename strip_dups2<H<Ss...>,typename setadd2<H<Ss...>,H<Ts...>,R>::type,Rs...>::type type;
	};

	template<typename> struct typelistsize;
	template<template<typename...> class H, typename... Ts>
	struct typelistsize<H<Ts...>> {
		enum {value=sizeof...(Ts)};
	};

	template<typename L1, typename L2, bool firstsmaller = (typelistsize<L1>::value <= typelistsize<L2>::value)>
			struct concat;
	
	template<template<typename...> class H, typename... Ts>
	struct concat<H<Ts...>,H<>,false> {
		typedef H<Ts...> type;
	};

	template<template<typename...> class H, typename... Rs, typename T, typename... Ts>
	struct concat<H<Rs...>,H<T,Ts...>,false> {
		typedef typename concat<H<Rs...,T>,H<Ts...>,false>::type type;
	};

	template<template<typename...> class H, typename... Ts>
	struct concat<H<>,H<Ts...>,true> {
		typedef H<Ts...> type;
	};

	template<template<typename...> class H, typename... Rs, typename T, typename... Ts>
	struct concat<H<T,Ts...>,H<Rs...>,true> {
		typedef typename concat<H<Ts...>,H<Rs...,T>,true>::type type;
	};

	template<typename L1,typename L2, bool firstsmaller = (typelistsize<L1>::value <= typelistsize<L2>::value)>
			struct flattenmerge;

	template<template<typename...> class H, typename... Rs, typename... Ss>
	struct flattenmerge<H<Rs...>,H<Ss...>,true> {
		typedef typename concat<H<Rs...>,typename strip_dups2<H<Rs...>,H<>,Ss...>::type>::type type;
	};

	template<template<typename...> class H, typename... Rs, typename... Ss>
	struct flattenmerge<H<Rs...>,H<Ss...>,false> {
		typedef typename concat<H<Ss...>,typename strip_dups2<H<Ss...>,H<>,Rs...>::type>::type type;
	};

	template<typename...> struct flattenandadd;

	template<template<typename...> class H, typename... Rs>
	struct flattenandadd<H<Rs...>> {
		typedef H<Rs...> type;
	};

	template<template<typename...> class H, typename... Rs, typename... Ss, typename... Ts>
	struct flattenandadd<H<Rs...>,H<Ss...>,Ts...> {
		typedef typename flattenandadd<typename flattenmerge<H<Rs...>,H<Ss...>>::type,Ts...>::type type;
	};

	template<template<typename...> class H, typename... Rs, typename T, typename... Ts>
	struct flattenandadd<H<Rs...>,T,Ts...> {
		typedef typename flattenandadd<typename setadd<H,H<Rs...>,T>::type,Ts...>::type type;
	};
	
	//-----------------------------

	template<typename...> struct splitargtype_odds;
	template<typename...> struct splitargtype_evens;

	template<typename... Rs, template<typename...> class H>
	struct splitargtype_odds<H<Rs...>> {
		typedef typename splittype_odds<H<>,Rs...>::type type;
	};

	template<typename... Rs, template<typename...> class H>
	struct splitargtype_evens<H<Rs...>> {
		typedef typename splittype_evens<H<>,Rs...>::type type;
	};

	//-----------------------------

	template<typename...> struct flattenothertype;

	template<typename... Rs, template<typename...> class H,
				template<typename...> class H2,
			typename T1, typename... Ts>
	struct flattenothertype<H2<>,H<Rs...>,T1,Ts...> {
		typedef typename flattenothertype<H2<>,H<Rs...,T1>,Ts...>::type type;
	};
		
	template<typename... Rs, template<typename...> class H,
				template<typename...> class H2,
			typename... Ss, typename... Ts>
	struct flattenothertype<H2<>,H<Rs...>,H2<Ss...>,Ts...> {
		typedef typename flattenothertype<H2<>,H<Rs...,Ss...>,Ts...>::type type;
	};

	template<typename... Rs, template<typename...> class H,
				template<typename...> class H2>
	struct flattenothertype<H2<>,H<Rs...>> {
		typedef H<Rs...> type;
	};

	template<typename...> struct flattentype;

	template<typename... Rs, template<typename...> class H, typename... Ts>
	struct flattentype<H<Rs...>,Ts...> {
		typedef typename flattenothertype<H<>,H<Rs...>,Ts...>::type type;
	};

	template<typename...> struct flattenargs;

	template<typename... Rs, template<typename...> class H>
	struct flattenargs<H<Rs...>> {
		typedef typename flattentype<H<>,Rs...>::type type;
	};
}
#endif
