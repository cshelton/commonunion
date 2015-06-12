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

	//-----------------------------

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

	/*
	template<typename R1, template<typename...> class H>
	struct isargtype<R1,H<>> {
		enum {value=0 };
	};
	*/

	//-----------------------------

	template<typename...> struct strip_dups;

	template<typename... Rs, template<typename...> class H,
			typename T1, typename... Ts>
	struct strip_dups<H<Rs...>,T1,Ts...> {
		typedef typename std::conditional<isargtype<T1,H<Rs...>>::value,
				typename strip_dups<H<Rs...>,Ts...>::type,
				typename strip_dups<H<Rs...,T1>,Ts...>::type>::type type;
	};

	template<typename... Rs, template<typename...> class H>
	struct strip_dups<H<Rs...>> {
		typedef H<Rs...> type;
	};

	template<typename...> struct stripdupargs;

	template<typename... Rs, template<typename...> class H>
	struct stripdupargs<H<Rs...>> {
		typedef typename strip_dups<H<>,Rs...>::type type;
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