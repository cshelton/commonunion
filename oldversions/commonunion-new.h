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

#define CUEXPRSELECT3(uname,cond,res0,res1,res2) \
     ((cond>=2) \
		? (uname<typename std::decay<decltype(res0)>::type, \
                              typename std::decay<decltype(res1)>::type, \
                              typename std::decay<decltype(res2)>::type> \
					{res2}) \
		: ((cond%1) ? uname<typename std::decay<decltype(res0)>::type, \
                              typename std::decay<decltype(res1)>::type, \
                              typename std::decay<decltype(res2)>::type> \
					{res1} \
				: uname<typename std::decay<decltype(res0)>::type, \
                              typename std::decay<decltype(res1)>::type, \
                              typename std::decay<decltype(res2)>::type> \
					{res0})) \

#define CUEXPRSELECT4(uname,cond,res0,res1,res2,res3) \
     ((cond>=2) \
		? ((cond%1) ? uname<typename std::decay<decltype(res0)>::type, \
                              typename std::decay<decltype(res1)>::type, \
                              typename std::decay<decltype(res2)>::type, \
                              typename std::decay<decltype(res3)>::type> \
					{res3} \
				: uname<typename std::decay<decltype(res0)>::type, \
                              typename std::decay<decltype(res1)>::type, \
                              typename std::decay<decltype(res2)>::type, \
                              typename std::decay<decltype(res3)>::type> \
					{res2}) \
		: ((cond%1) ? uname<typename std::decay<decltype(res0)>::type, \
                              typename std::decay<decltype(res1)>::type, \
                              typename std::decay<decltype(res2)>::type, \
                              typename std::decay<decltype(res3)>::type> \
					{res1} \
				: uname<typename std::decay<decltype(res0)>::type, \
                              typename std::decay<decltype(res1)>::type, \
                              typename std::decay<decltype(res2)>::type, \
                              typename std::decay<decltype(res3)>::type> \
					{res0})) \

namespace commonunion {

	using splittype::isargtype;

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

	template<typename...> struct alltrivdestruct;

	template<typename T>
	struct alltrivdestruct<T> {
		enum { value = std::is_trivially_destructible<T>::value };
	};
	template<typename T1, typename T2, typename... Ts>
	struct alltrivdestruct<T1,T2,Ts...> {
		enum { value = std::is_trivially_destructible<T1>::value
				&& alltrivdestruct<T2,Ts...>::value };
	};

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
	//
	
	template<std::size_t I>
	struct numT {};

	template<std::size_t MAXI>
	struct condbuildT {
		template<typename RetT, typename... Ts>
		constexpr RetT exec(std::size_t i, Ts &&...arg) {
			switch(i) {
				case MAXI: return {numT<MAXI>{},std::forward<Ts>(arg)...};
				default: return condbuildT<MAXI-1>{}.template exec<RetT>(i,std::forward<Ts>(arg)...);
			}
		}
	};

	template<>
	struct condbuildT<0> {
		template<typename RetT, typename... Ts>
		constexpr RetT exec(std::size_t i, Ts &&...arg) {
			return {numT<0>{},std::forward<Ts>(arg)...};
		}
	};

	

	template<typename RetT, std::size_t MAXI, typename... Ts>
	constexpr RetT condbuild(std::size_t i, Ts &&...arg) {
		return condbuildT<MAXI>{}.template exec<RetT>(i,std::forward<Ts>(arg)...);
	}

	struct dummyT {};

	template<typename...> struct trivunion;
	template<typename...> struct nontrivunion;

	template<typename T1, typename T2>
	struct trivunion<T1,T2> {
		union {
			dummyT dummy;
			T1 t1;
			T2 t2;
		};
		constexpr trivunion(): dummy() {}
		template<typename... Ts>
		constexpr trivunion(numT<0>, Ts &&...args) : t1(std::forward<Ts>(args)...) {}
		template<typename... Ts>
		constexpr trivunion(numT<1>, Ts &&...args) : t2(std::forward<Ts>(args)...) {}
		template<typename... Ts>
		constexpr trivunion(unsigned char usesecond, Ts &&...args)
			: trivunion(condbuild<trivunion,1>(usesecond,std::forward<Ts>(args)...)) {}

		template<typename... X>
		void assign(bool wassecond, bool tosecond, X &&...x) {
			if (tosecond) t2.assign(std::forward<X>(x)...);
			else t1.assign(std::forward<X>(x)...);
		}
		template<typename X>
		void assignreal(bool wassecond, bool tosecond, X &&x) {
			if (tosecond) t2=std::forward<X>(x);
			else t1=std::forward<X>(x);
		}
	};

	template<typename T1, typename T2,typename T3>
	struct trivunion<T1,T2,T3> {
		union {
			dummyT dummy;
			T1 t1;
			T2 t2;
			T3 t3;
		};
		constexpr trivunion(): dummy() {}
		template<typename... Ts>
		constexpr trivunion(numT<0>, Ts &&...args) : t1(std::forward<Ts>(args)...) {}
		template<typename... Ts>
		constexpr trivunion(numT<1>, Ts &&...args) : t2(std::forward<Ts>(args)...) {}
		template<typename... Ts>
		constexpr trivunion(numT<2>, Ts &&...args) : t3(std::forward<Ts>(args)...) {}
		template<typename... Ts>
		constexpr trivunion(unsigned char place, Ts &&...args)
			: trivunion(condbuild<trivunion,2>(place,std::forward<Ts>(args)...)) {}

		template<typename... X>
		void assign(unsigned char wasplace, unsigned char toplace, X &&...x) {
			switch(toplace) {
				case 0: t1.assign(std::forward<X>(x)...); break;
				case 1: t2.assign(std::forward<X>(x)...); break;
				default: t3.assign(std::forward<X>(x)...); break;
			}
		}
		template<typename X>
		void assignreal(unsigned char wasplace, unsigned char toplace, X &&x) {
			switch(toplace) {
				case 0: t1=std::forward<X>(x); break;
				case 1: t2=std::forward<X>(x); break;
				default: t3=std::forward<X>(x); break;
			}
		}
	};
	template<typename T1, typename T2,typename T3,typename T4>
	struct trivunion<T1,T2,T3,T4> {
		union {
			dummyT dummy;
			T1 t1;
			T2 t2;
			T3 t3;
			T4 t4;
		};
		constexpr trivunion(): dummy() {}
		template<typename... Ts>
		constexpr trivunion(numT<0>, Ts &&...args) : t1(std::forward<Ts>(args)...) {}
		template<typename... Ts>
		constexpr trivunion(numT<1>, Ts &&...args) : t2(std::forward<Ts>(args)...) {}
		template<typename... Ts>
		constexpr trivunion(numT<2>, Ts &&...args) : t3(std::forward<Ts>(args)...) {}
		template<typename... Ts>
		constexpr trivunion(numT<3>, Ts &&...args) : t4(std::forward<Ts>(args)...) {}
		template<typename... Ts>
		constexpr trivunion(unsigned char place, Ts &&...args)
			: trivunion(condbuild<trivunion,3>(place,std::forward<Ts>(args)...)) {}

		template<typename... X>
		void assign(unsigned char wasplace, unsigned char toplace, X &&...x) {
			switch(toplace) {
				case 0: t1.assign(std::forward<X>(x)...); break;
				case 1: t2.assign(std::forward<X>(x)...); break;
				case 2: t3.assign(std::forward<X>(x)...); break;
				default: t4.assign(std::forward<X>(x)...); break;
			}
		}
		template<typename X>
		void assignreal(unsigned char wasplace, unsigned char toplace, X &&x) {
			switch(toplace) {
				case 0: t1=std::forward<X>(x); break;
				case 1: t2=std::forward<X>(x); break;
				case 2: t3=std::forward<X>(x); break;
				default: t4=std::forward<X>(x); break;
			}
		}
	};

	template<typename T1, typename T2>
	struct nontrivunion<T1,T2> {
		union {
			dummyT dummy;
			T1 t1;
			T2 t2;
		};
		constexpr nontrivunion(): dummy() {}
		template<typename... Ts>
		constexpr nontrivunion(numT<1>, Ts &&...args) : t2(std::forward<Ts>(args)...) {}
		template<typename... Ts>
		constexpr nontrivunion(numT<0>, Ts &&...args) : t1(std::forward<Ts>(args)...) {}
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
		template<typename X>
		void assignreal(bool wassecond, bool tosecond, X &&x) {
			if (tosecond) {
				if (wassecond) t2=std::forward<X>(x);
				else {
					t1.~T1();
					new (&t2) T2(std::forward<X>(x));
				}
			} else {
				if (!wassecond) t1=std::forward<X>(x);
				else {
					t2.~T2();
					new (&t1) T1(std::forward<X>(x));
				}
			}
		}
	};
	template<typename T1, typename T2, typename T3>
	struct nontrivunion<T1,T2,T3> {
		union {
			dummyT dummy;
			T1 t1;
			T2 t2;
			T3 t3;
		};
		constexpr nontrivunion(): dummy() {}
		template<typename... Ts>
		constexpr nontrivunion(numT<2>, Ts &&...args) : t3(std::forward<Ts>(args)...) {}
		template<typename... Ts>
		constexpr nontrivunion(numT<1>, Ts &&...args) : t2(std::forward<Ts>(args)...) {}
		template<typename... Ts>
		constexpr nontrivunion(numT<0>, Ts &&...args) : t1(std::forward<Ts>(args)...) {}
		template<typename... Ts>
		constexpr nontrivunion(unsigned char place, Ts &&...args) : dummy() {
			switch(place) {
				case 0: new (&t1) T1(std::forward<Ts>(args)...); break;
				case 1: new (&t2) T2(std::forward<Ts>(args)...); break;
				default: new (&t3) T3(std::forward<Ts>(args)...); break;
			}
		}

		~nontrivunion() {}

		void predelete(unsigned char place) {
			switch(place) {
				case 0: t1.~T1(); break;
				case 1: t2.~T2(); break;
				default: t3.~T3(); break;
			}
		}

		template<typename... X>
		void assign(unsigned char wasplace, unsigned char toplace, X &&...x) {
			switch(toplace) {
				case 0:
					switch(wasplace) {
						case 0: t1.assign(std::forward<X>(x)...);
							break;
						case 1: t1.~T1();
							new (&t2) T2(std::forward<X>(x)...);
							break;
						default: t1.~T1();
							new (&t3) T3(std::forward<X>(x)...);
							break;
					}
					break;
				case 1:
					switch(wasplace) {
						case 1: t2.assign(std::forward<X>(x)...);
							break;
						case 0: t2.~T2();
							new (&t1) T1(std::forward<X>(x)...);
							break;
						default: t2.~T2();
							new (&t3) T3(std::forward<X>(x)...);
							break;
					}
					break;
				default:
					switch(wasplace) {
						case 2: t3.assign(std::forward<X>(x)...);
							break;
						case 0: t3.~T3();
							new (&t1) T1(std::forward<X>(x)...);
							break;
						default: t3.~T3();
							new (&t2) T2(std::forward<X>(x)...);
							break;
					}
					break;
			}
		}
		template<typename X>
		void assignreal(unsigned char wasplace, unsigned char toplace, X &&x) {
			switch(toplace) {
				case 0:
					switch(wasplace) {
						case 0: t1=std::forward<X>(x);
							break;
						case 1: t1.~T1();
							new (&t2) T2(std::forward<X>(x));
							break;
						default: t1.~T1();
							new (&t3) T3(std::forward<X>(x));
							break;
					}
					break;
				case 1:
					switch(wasplace) {
						case 1: t2=std::forward<X>(x);
							break;
						case 0: t2.~T2();
							new (&t1) T1(std::forward<X>(x));
							break;
						default: t2.~T2();
							new (&t3) T3(std::forward<X>(x));
							break;
					}
					break;
				default:
					switch(wasplace) {
						case 2: t3=std::forward<X>(x);
							break;
						case 0: t3.~T3();
							new (&t1) T1(std::forward<X>(x));
							break;
						default: t3.~T3();
							new (&t2) T2(std::forward<X>(x));
							break;
					}
					break;
			}
		}
	};

	template<typename T1, typename T2, typename T3,typename T4>
	struct nontrivunion<T1,T2,T3,T4> {
		union {
			dummyT dummy;
			T1 t1;
			T2 t2;
			T3 t3;
			T4 t4;
		};
		constexpr nontrivunion(): dummy() {}
		template<typename... Ts>
		constexpr nontrivunion(numT<3>, Ts &&...args) : t4(std::forward<Ts>(args)...) {}
		template<typename... Ts>
		constexpr nontrivunion(numT<2>, Ts &&...args) : t3(std::forward<Ts>(args)...) {}
		template<typename... Ts>
		constexpr nontrivunion(numT<1>, Ts &&...args) : t2(std::forward<Ts>(args)...) {}
		template<typename... Ts>
		constexpr nontrivunion(numT<0>, Ts &&...args) : t1(std::forward<Ts>(args)...) {}
		template<typename... Ts>
		constexpr nontrivunion(unsigned char place, Ts &&...args) : dummy() {
			switch(place) {
				case 0: new (&t1) T1(std::forward<Ts>(args)...); break;
				case 1: new (&t2) T2(std::forward<Ts>(args)...); break;
				case 2: new (&t3) T3(std::forward<Ts>(args)...); break;
				default: new (&t4) T4(std::forward<Ts>(args)...); break;
			}
		}

		~nontrivunion() {}

		void predelete(unsigned char place) {
			switch(place) {
				case 0: t1.~T1(); break;
				case 1: t2.~T2(); break;
				case 2: t3.~T3(); break;
				default: t4.~T4(); break;
			}
		}

		template<typename... X>
		void assign(unsigned char wasplace, unsigned char toplace, X &&...x) {
			switch(toplace) {
				case 0:
					switch(wasplace) {
						case 0: t1.assign(std::forward<X>(x)...);
							break;
						case 1: t1.~T1();
							new (&t2) T2(std::forward<X>(x)...);
							break;
						case 2: t1.~T1();
							new (&t3) T3(std::forward<X>(x)...);
							break;
						default: t1.~T1();
							new (&t4) T4(std::forward<X>(x)...);
							break;
					}
					break;
				case 1:
					switch(wasplace) {
						case 1: t2.assign(std::forward<X>(x)...);
							break;
						case 0: t2.~T2();
							new (&t1) T1(std::forward<X>(x)...);
							break;
						case 2: t2.~T2();
							new (&t3) T3(std::forward<X>(x)...);
							break;
						default: t2.~T2();
							new (&t4) T4(std::forward<X>(x)...);
							break;
					}
					break;
				case 2:
					switch(wasplace) {
						case 2: t3.assign(std::forward<X>(x)...);
							break;
						case 0: t3.~T3();
							new (&t1) T1(std::forward<X>(x)...);
							break;
						case 1: t3.~T3();
							new (&t2) T2(std::forward<X>(x)...);
							break;
						default: t3.~T3();
							new (&t4) T4(std::forward<X>(x)...);
							break;
					}
					break;
				default:
					switch(wasplace) {
						case 3: t4.assign(std::forward<X>(x)...);
							break;
						case 0: t4.~T4();
							new (&t1) T1(std::forward<X>(x)...);
							break;
						case 1: t4.~T4();
							new (&t2) T2(std::forward<X>(x)...);
							break;
						default: t4.~T4();
							new (&t3) T3(std::forward<X>(x)...);
							break;
					}
					break;
			}
		}
		template<typename X>
		void assignreal(unsigned char wasplace, unsigned char toplace, X &&x) {
			switch(toplace) {
				case 0:
					switch(wasplace) {
						case 0: t1=std::forward<X>(x);
							break;
						case 1: t1.~T1();
							new (&t2) T2(std::forward<X>(x));
							break;
						case 2: t1.~T1();
							new (&t3) T3(std::forward<X>(x));
							break;
						default: t1.~T1();
							new (&t4) T4(std::forward<X>(x));
							break;
					}
					break;
				case 1:
					switch(wasplace) {
						case 1: t2=std::forward<X>(x);
							break;
						case 0: t2.~T2();
							new (&t1) T1(std::forward<X>(x));
							break;
						case 2: t2.~T2();
							new (&t3) T3(std::forward<X>(x));
							break;
						default: t2.~T2();
							new (&t4) T4(std::forward<X>(x));
							break;
					}
					break;
				case 2:
					switch(wasplace) {
						case 2: t3=std::forward<X>(x);
							break;
						case 0: t3.~T3();
							new (&t1) T1(std::forward<X>(x));
							break;
						case 1: t3.~T3();
							new (&t2) T2(std::forward<X>(x));
							break;
						default: t3.~T3();
							new (&t4) T4(std::forward<X>(x));
							break;
					}
					break;
				default:
					switch(wasplace) {
						case 3: t4=std::forward<X>(x);
							break;
						case 0: t4.~T4();
							new (&t1) T1(std::forward<X>(x));
							break;
						case 1: t4.~T4();
							new (&t2) T2(std::forward<X>(x));
							break;
						default: t4.~T4();
							new (&t3) T3(std::forward<X>(x));
							break;
					}
					break;
			}
		}
	};


	template<typename... Ts>
	using delunion = typename std::conditional<
				alltrivdestruct<Ts...>::value,
					trivunion<Ts...>,
					nontrivunion<Ts...>>::type;


	//------------------
	
	template<typename T>
	constexpr unsigned char lastbit(const T &x) { return x&3; }

	template<typename T>
	constexpr T restbits(const T &x) { return x>>2; }
	
	struct initdirect {};

	template<typename...> struct cu_node_base;

	template<typename T1, typename T2, typename T3, typename T4, typename... Ts>
	struct cu_node_base<T1,T2,T3,T4,Ts...> {
		using type1 = typename splittype::splitargtype0<cu_node_base<T1,T2,T3,T4,Ts...>>::type;
		using type2 = typename splittype::splitargtype1<cu_node_base<T1,T2,T3,T4,Ts...>>::type;
		using type3 = typename splittype::splitargtype2<cu_node_base<T1,T2,T3,T4,Ts...>>::type;
		using type4 = typename splittype::splitargtype3<cu_node_base<T1,T2,T3,T4,Ts...>>::type;
		using itype = typename argstoindextype<T1,T2,T3,T4,Ts...>::type;

		template<typename T, typename EN=void>
		struct index { enum {value=0}; };
		template<typename T>
		struct index<T,typename std::enable_if<
						isargtype<decay<T>,type1>::value>::type> {
			enum {value=type1::template index<T>::value*4}; };
		template<typename T>
		struct index<T,typename std::enable_if<
						isargtype<decay<T>,type2>::value>::type> {
			enum {value=1+type2::template index<T>::value*4}; };
		template<typename T>
		struct index<T,typename std::enable_if<
						isargtype<decay<T>,type3>::value>::type> {
			enum {value=2+type2::template index<T>::value*4}; };
		template<typename T>
		struct index<T,typename std::enable_if<
						isargtype<decay<T>,type4>::value>::type> {
			enum {value=3+type2::template index<T>::value*4}; };

		constexpr cu_node_base(initdirect,dummyT) : x() {};

		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type1>::value>::type *En=nullptr>
		constexpr cu_node_base(initdirect,T &&t) : x(numT<0>{},initdirect{},std::forward<T>(t)) {}
		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type2>::value>::type *En=nullptr>
		constexpr cu_node_base(initdirect,T &&t) : x(numT<1>{},initdirect{},std::forward<T>(t)) {}
		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type3>::value>::type *En=nullptr>
		constexpr cu_node_base(initdirect,T &&t) : x(numT<2>{},initdirect{},std::forward<T>(t)) {}
		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type4>::value>::type *En=nullptr>
		constexpr cu_node_base(initdirect,T &&t) : x(numT<3>{},initdirect{},std::forward<T>(t)) {}

		template<typename... Ss>
		constexpr cu_node_base(itype place, Ss &&...arg)
			: x(lastbit(place),restbits(place),std::forward<Ss>(arg)...) {}

		template<typename... Ss>
		constexpr cu_node_base(itype fromplace, itype toplace, Ss &&...arg)
					: cu_node_base(toplace,std::forward<Ss>(arg)...) {}

		template<typename... V>
		void assign(itype fromplace, itype toplace, V &&...v) {
			x.assign(lastbit(fromplace),lastbit(toplace),
			  restbits(fromplace),restbits(toplace),std::forward<V>(v)...);
		}

		template<typename... Ss>
		static constexpr typename cu_node_base<Ss...>::itype reindex(itype i) {
			switch(lastbit(i)) {
				case 0: return type1::template reindex<Ss...>(restbits(i));
				case 1: return type2::template reindex<Ss...>(restbits(i));
				case 2: return type3::template reindex<Ss...>(restbits(i));
				default: return type4::template reindex<Ss...>(restbits(i));
			}
		}

		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type1>::value>::type *En=nullptr>
		constexpr const T &get() const { return x.t1.template get<T>(); }
		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type1>::value>::type *En=nullptr>
		T &get() & { return x.t1.template get<T>(); }
		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type1>::value>::type *En=nullptr>
		T &&get() && { return std::move(x.t1.template get<T>()); }

		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type2>::value>::type *En=nullptr>
		constexpr const T &get() const { return x.t2.template get<T>(); }
		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type2>::value>::type *En=nullptr>
		T &get() & { return x.t2.template get<T>(); }
		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type2>::value>::type *En=nullptr>
		T &&get() && { return std::move(std::move(x).t2.template get<T>()); }

		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type3>::value>::type *En=nullptr>
		constexpr const T &get() const { return x.t3.template get<T>(); }
		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type3>::value>::type *En=nullptr>
		T &get() & { return x.t3.template get<T>(); }
		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type3>::value>::type *En=nullptr>
		T &&get() && { return std::move(x.t3.template get<T>()); }

		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type4>::value>::type *En=nullptr>
		constexpr const T &get() const { return x.t4.template get<T>(); }
		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type4>::value>::type *En=nullptr>
		T &get() & { return x.t4.template get<T>(); }
		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type4>::value>::type *En=nullptr>
		T &&get() && { return std::move(std::move(x).t4.template get<T>()); }

		template<typename T,
			typename std::enable_if<!isargtype<decay<T>,type1>::value
						&& !isargtype<decay<T>,type2>::value
						&& !isargtype<decay<T>,type3>::value
						&& !isargtype<decay<T>,type4>::value>::type *En=nullptr>
		constexpr const T &get() const { throw std::logic_error("invalid type conversion"); }
		template<typename T,
			typename std::enable_if<!isargtype<decay<T>,type1>::value
						&& !isargtype<decay<T>,type2>::value
						&& !isargtype<decay<T>,type3>::value
						&& !isargtype<decay<T>,type4>::value>::type *En=nullptr>
		constexpr T &get() & { throw std::logic_error("invalid type conversion"); }
		template<typename T,
			typename std::enable_if<!isargtype<decay<T>,type1>::value
						&& !isargtype<decay<T>,type2>::value
						&& !isargtype<decay<T>,type3>::value
						&& !isargtype<decay<T>,type4>::value>::type *En=nullptr>
		constexpr T &&get() && { throw std::logic_error("invalid type conversion"); }

		constexpr void predelete(itype) {}

		template<typename F, typename... Ss>
		constexpr auto callfn(itype i, F &&f, Ss &&...args) const {
			switch(lastbit(i)) {
				case 0: return x.t1.callfn(restbits(i),std::forward<F>(f),std::forward<Ss>(args)...);
				case 1: return x.t2.callfn(restbits(i),std::forward<F>(f),std::forward<Ss>(args)...);
				case 2: return x.t3.callfn(restbits(i),std::forward<F>(f),std::forward<Ss>(args)...);
				default: return x.t4.callfn(restbits(i),std::forward<F>(f),std::forward<Ss>(args)...);
			}
		}
		template<typename F, typename... Ss>
		constexpr auto callfn(itype i, F &&f, Ss &&...args) {
			switch(lastbit(i)) {
				case 0: return x.t1.callfn(restbits(i),std::forward<F>(f),std::forward<Ss>(args)...);
				case 1: return x.t2.callfn(restbits(i),std::forward<F>(f),std::forward<Ss>(args)...);
				case 2: return x.t3.callfn(restbits(i),std::forward<F>(f),std::forward<Ss>(args)...);
				default: return x.t4.callfn(restbits(i),std::forward<F>(f),std::forward<Ss>(args)...);
			}
		}
		template<template<typename...> class R, typename F, typename... Ss>
		constexpr auto callfnunion(itype i, F &&f, Ss &&...args) const {
			return CUEXPRSELECT4(R,
				(lastbit(i)),
					(x.t1.template callfnunion<R>(restbits(i),
						std::forward<F>(f),
						std::forward<Ss>(args)...)),
					(x.t2.template callfnunion<R>(restbits(i),
						std::forward<F>(f),
						std::forward<Ss>(args)...)),
					(x.t3.template callfnunion<R>(restbits(i),
						std::forward<F>(f),
						std::forward<Ss>(args)...)),
					(x.t4.template callfnunion<R>(restbits(i),
						std::forward<F>(f),
						std::forward<Ss>(args)...)));
		}
		template<template<typename...> class R, typename F, typename... Ss>
		constexpr auto callfnunion(itype i, F &&f, Ss &&...args) {
			return CUEXPRSELECT4(R,
				(lastbit(i)),
					(x.t1.template callfnunion<R>(restbits(i),
						std::forward<F>(f),
						std::forward<Ss>(args)...)),
					(x.t2.template callfnunion<R>(restbits(i),
						std::forward<F>(f),
						std::forward<Ss>(args)...)),
					(x.t3.template callfnunion<R>(restbits(i),
						std::forward<F>(f),
						std::forward<Ss>(args)...)),
					(x.t4.template callfnunion<R>(restbits(i),
						std::forward<F>(f),
						std::forward<Ss>(args)...)));
		}

		delunion<type1,type2,type3,type4> x;
	};

	template<typename T1, typename T2, typename T3>
	struct cu_node_base<T1,T2,T3> {
		using type1 = cu_node_base<T1>;
		using type2 = cu_node_base<T2>;
		using type3 = cu_node_base<T3>;
		using itype = typename argstoindextype<T1,T2,T3>::type;

		template<typename T, typename EN=void>
		struct index { enum {value=0}; };
		template<typename T>
		struct index<T,typename std::enable_if<
						isargtype<decay<T>,type1>::value>::type> {
			enum {value=type1::template index<T>::value*4}; };
		template<typename T>
		struct index<T,typename std::enable_if<
						isargtype<decay<T>,type2>::value>::type> {
			enum {value=1+type2::template index<T>::value*4}; };
		template<typename T>
		struct index<T,typename std::enable_if<
						isargtype<decay<T>,type3>::value>::type> {
			enum {value=2+type2::template index<T>::value*4}; };

		constexpr cu_node_base(initdirect,dummyT) : x() {};

		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type1>::value>::type *En=nullptr>
		constexpr cu_node_base(initdirect,T &&t) : x(numT<0>{},initdirect{},std::forward<T>(t)) {}
		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type2>::value>::type *En=nullptr>
		constexpr cu_node_base(initdirect,T &&t) : x(numT<1>{},initdirect{},std::forward<T>(t)) {}
		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type3>::value>::type *En=nullptr>
		constexpr cu_node_base(initdirect,T &&t) : x(numT<2>{},initdirect{},std::forward<T>(t)) {}

		template<typename... Ss>
		constexpr cu_node_base(itype place, Ss &&...arg)
			: x(lastbit(place),restbits(place),std::forward<Ss>(arg)...) {}

		template<typename... Ss>
		constexpr cu_node_base(itype fromplace, itype toplace, Ss &&...arg)
					: cu_node_base(toplace,std::forward<Ss>(arg)...) {}

		template<typename... V>
		void assign(itype fromplace, itype toplace, V &&...v) {
			x.assign(lastbit(fromplace),lastbit(toplace),
			  restbits(fromplace),restbits(toplace),std::forward<V>(v)...);
		}

		template<typename... Ss>
		static constexpr typename cu_node_base<Ss...>::itype reindex(itype i) {
			switch(lastbit(i)) {
				case 0: return type1::template reindex<Ss...>(restbits(i));
				case 1: return type2::template reindex<Ss...>(restbits(i));
				default: return type3::template reindex<Ss...>(restbits(i));
			}
		}

		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type1>::value>::type *En=nullptr>
		constexpr const T &get() const { return x.t1.template get<T>(); }
		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type1>::value>::type *En=nullptr>
		T &get() & { return x.t1.template get<T>(); }
		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type1>::value>::type *En=nullptr>
		T &&get() && { return std::move(x.t1.template get<T>()); }

		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type2>::value>::type *En=nullptr>
		constexpr const T &get() const { return x.t2.template get<T>(); }
		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type2>::value>::type *En=nullptr>
		T &get() & { return x.t2.template get<T>(); }
		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type2>::value>::type *En=nullptr>
		T &&get() && { return std::move(std::move(x).t2.template get<T>()); }

		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type3>::value>::type *En=nullptr>
		constexpr const T &get() const { return x.t3.template get<T>(); }
		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type3>::value>::type *En=nullptr>
		T &get() & { return x.t3.template get<T>(); }
		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type3>::value>::type *En=nullptr>
		T &&get() && { return std::move(x.t3.template get<T>()); }

		template<typename T,
			typename std::enable_if<!isargtype<decay<T>,type1>::value
						&& !isargtype<decay<T>,type2>::value
						&& !isargtype<decay<T>,type3>::value>::type *En=nullptr>
		constexpr const T &get() const { throw std::logic_error("invalid type conversion"); }
		template<typename T,
			typename std::enable_if<!isargtype<decay<T>,type1>::value
						&& !isargtype<decay<T>,type2>::value
						&& !isargtype<decay<T>,type3>::value>::type *En=nullptr>
		constexpr T &get() & { throw std::logic_error("invalid type conversion"); }
		template<typename T,
			typename std::enable_if<!isargtype<decay<T>,type1>::value
						&& !isargtype<decay<T>,type2>::value
						&& !isargtype<decay<T>,type3>::value>::type *En=nullptr>
		constexpr T &&get() && { throw std::logic_error("invalid type conversion"); }

		constexpr void predelete(itype) {}

		template<typename F, typename... Ss>
		constexpr auto callfn(itype i, F &&f, Ss &&...args) const {
			switch(lastbit(i)) {
				case 0: return x.t1.callfn(restbits(i),std::forward<F>(f),std::forward<Ss>(args)...);
				case 1: return x.t2.callfn(restbits(i),std::forward<F>(f),std::forward<Ss>(args)...);
				default: return x.t3.callfn(restbits(i),std::forward<F>(f),std::forward<Ss>(args)...);
			}
		}
		template<typename F, typename... Ss>
		constexpr auto callfn(itype i, F &&f, Ss &&...args) {
			switch(lastbit(i)) {
				case 0: return x.t1.callfn(restbits(i),std::forward<F>(f),std::forward<Ss>(args)...);
				case 1: return x.t2.callfn(restbits(i),std::forward<F>(f),std::forward<Ss>(args)...);
				default: return x.t3.callfn(restbits(i),std::forward<F>(f),std::forward<Ss>(args)...);
			}
		}
		template<template<typename...> class R, typename F, typename... Ss>
		constexpr auto callfnunion(itype i, F &&f, Ss &&...args) const {
			return CUEXPRSELECT3(R,
				(lastbit(i)),
					(x.t1.template callfnunion<R>(restbits(i),
						std::forward<F>(f),
						std::forward<Ss>(args)...)),
					(x.t2.template callfnunion<R>(restbits(i),
						std::forward<F>(f),
						std::forward<Ss>(args)...)),
					(x.t3.template callfnunion<R>(restbits(i),
						std::forward<F>(f),
						std::forward<Ss>(args)...)));
		}
		template<template<typename...> class R, typename F, typename... Ss>
		constexpr auto callfnunion(itype i, F &&f, Ss &&...args) {
			return CUEXPRSELECT3(R,
				(lastbit(i)),
					(x.t1.template callfnunion<R>(restbits(i),
						std::forward<F>(f),
						std::forward<Ss>(args)...)),
					(x.t2.template callfnunion<R>(restbits(i),
						std::forward<F>(f),
						std::forward<Ss>(args)...)),
					(x.t3.template callfnunion<R>(restbits(i),
						std::forward<F>(f),
						std::forward<Ss>(args)...)));
		}

		delunion<type1,type2,type3> x;
	};

	template<typename T1, typename T2>
	struct cu_node_base<T1,T2> {
		using type1 = cu_node_base<T1>;
		using type2 = cu_node_base<T2>;
		using itype = typename argstoindextype<T1,T2>::type;

		template<typename T, typename EN=void>
		struct index { enum {value=0}; };
		template<typename T>
		struct index<T,typename std::enable_if<
						isargtype<decay<T>,type1>::value>::type> {
			enum {value=type1::template index<T>::value*4}; };
		template<typename T>
		struct index<T,typename std::enable_if<
						isargtype<decay<T>,type2>::value>::type> {
			enum {value=1+type2::template index<T>::value*4}; };

		constexpr cu_node_base(initdirect,dummyT) : x() {};

		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type1>::value>::type *En=nullptr>
		constexpr cu_node_base(initdirect,T &&t) : x(numT<0>{},initdirect{},std::forward<T>(t)) {}
		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type2>::value>::type *En=nullptr>
		constexpr cu_node_base(initdirect,T &&t) : x(numT<1>{},initdirect{},std::forward<T>(t)) {}

		template<typename... Ss>
		constexpr cu_node_base(itype place, Ss &&...arg)
			: x(lastbit(place),restbits(place),std::forward<Ss>(arg)...) {}

		template<typename... Ss>
		constexpr cu_node_base(itype fromplace, itype toplace, Ss &&...arg)
					: cu_node_base(toplace,std::forward<Ss>(arg)...) {}

		template<typename... V>
		void assign(itype fromplace, itype toplace, V &&...v) {
			x.assign(lastbit(fromplace),lastbit(toplace),
			  restbits(fromplace),restbits(toplace),std::forward<V>(v)...);
		}

		template<typename... Ss>
		static constexpr typename cu_node_base<Ss...>::itype reindex(itype i) {
			switch(lastbit(i)) {
				case 0: return type1::template reindex<Ss...>(restbits(i));
				default: return type2::template reindex<Ss...>(restbits(i));
			}
		}

		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type1>::value>::type *En=nullptr>
		constexpr const T &get() const { return x.t1.template get<T>(); }
		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type1>::value>::type *En=nullptr>
		T &get() & { return x.t1.template get<T>(); }
		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type1>::value>::type *En=nullptr>
		T &&get() && { return std::move(x.t1.template get<T>()); }

		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type2>::value>::type *En=nullptr>
		constexpr const T &get() const { return x.t2.template get<T>(); }
		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type2>::value>::type *En=nullptr>
		T &get() & { return x.t2.template get<T>(); }
		template<typename T,
			typename std::enable_if<isargtype<decay<T>,type2>::value>::type *En=nullptr>
		T &&get() && { return std::move(std::move(x).t2.template get<T>()); }

		template<typename T,
			typename std::enable_if<!isargtype<decay<T>,type1>::value
						&& !isargtype<decay<T>,type2>::value>::type *En=nullptr>
		constexpr const T &get() const { throw std::logic_error("invalid type conversion"); }
		template<typename T,
			typename std::enable_if<!isargtype<decay<T>,type1>::value
						&& !isargtype<decay<T>,type2>::value>::type *En=nullptr>
		constexpr T &get() & { throw std::logic_error("invalid type conversion"); }
		template<typename T,
			typename std::enable_if<!isargtype<decay<T>,type1>::value
						&& !isargtype<decay<T>,type2>::value>::type *En=nullptr>
		constexpr T &&get() && { throw std::logic_error("invalid type conversion"); }

		constexpr void predelete(itype) {}

		template<typename F, typename... Ss>
		constexpr auto callfn(itype i, F &&f, Ss &&...args) const {
			switch(lastbit(i)) {
				case 0: return x.t1.callfn(restbits(i),std::forward<F>(f),std::forward<Ss>(args)...);
				default: return x.t2.callfn(restbits(i),std::forward<F>(f),std::forward<Ss>(args)...);
			}
		}
		template<typename F, typename... Ss>
		constexpr auto callfn(itype i, F &&f, Ss &&...args) {
			switch(lastbit(i)) {
				case 0: return x.t1.callfn(restbits(i),std::forward<F>(f),std::forward<Ss>(args)...);
				default: return x.t2.callfn(restbits(i),std::forward<F>(f),std::forward<Ss>(args)...);
			}
		}
		template<template<typename...> class R, typename F, typename... Ss>
		constexpr auto callfnunion(itype i, F &&f, Ss &&...args) const {
			return CUEXPRSELECT(R,
				(lastbit(i)),
					(x.t1.template callfnunion<R>(restbits(i),
						std::forward<F>(f),
						std::forward<Ss>(args)...)),
					(x.t2.template callfnunion<R>(restbits(i),
						std::forward<F>(f),
						std::forward<Ss>(args)...)));
		}
		template<template<typename...> class R, typename F, typename... Ss>
		constexpr auto callfnunion(itype i, F &&f, Ss &&...args) {
			return CUEXPRSELECT(R,
				(lastbit(i)),
					(x.t1.template callfnunion<R>(restbits(i),
						std::forward<F>(f),
						std::forward<Ss>(args)...)),
					(x.t2.template callfnunion<R>(restbits(i),
						std::forward<F>(f),
						std::forward<Ss>(args)...)));
		}

		delunion<type1,type2> x;
	};

	template<typename T1>
	struct cu_node_base<T1> {
		using itype = typename argstoindextype<T1>::type;
		template<typename... Ss>
		using mybase = cu_node_base<Ss...>;

		template<typename T, typename EN=void>
		struct index { enum {value=0}; };

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
			

		template<typename... Ss>
		static constexpr typename cu_node_base<Ss...>::itype reindex(itype i) {
			return cu_node_base<Ss...>::template index<T1>::value;
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

		template<typename F, typename... Ss>
		constexpr auto callfn(itype i, F &&f, Ss &&...args) const {
			return f(t1,std::forward<Ss>(args)...);
		}
		template<typename F, typename... Ss>
		constexpr auto callfn(itype i, F &&f, Ss &&...args) {
			return f(t1,std::forward<Ss>(args)...);
		}
		template<template<typename...> class, typename F, typename... Ss>
		constexpr auto callfnunion(itype i, F &&f, Ss &&...args) const {
			return f(t1,std::forward<Ss>(args)...);
		}
		template<template<typename...> class, typename F, typename... Ss>
		constexpr auto callfnunion(itype i, F &&f, Ss &&...args) {
			return f(t1,std::forward<Ss>(args)...);
		}

		constexpr void predelete(itype) {}
		T1 t1;
	};

	//-------------------------------

	template<typename...> struct cu_node_nontriv;

	template<typename T1, typename T2, typename T3, typename T4, typename... Ts>
	struct cu_node_nontriv<T1,T2,T3,T4,Ts...> : cu_node_base<T1,T2,T3,T4,Ts...> {
		using B = cu_node_base<T1,T2,T3,T4,Ts...>;
		using B::B;
		using itype = typename B::itype;
		using type1 = typename B::type1;
		using type2 = typename B::type2;
		using type3 = typename B::type3;
		using type4 = typename B::type4;

		constexpr void predelete(itype i) {
			switch(lastbit(i)) {
				case 0: this->x.t1.predelete(restbits(i)); break;
				case 1: this->x.t2.predelete(restbits(i)); break;
				case 2: this->x.t3.predelete(restbits(i)); break;
				default: this->x.t4.predelete(restbits(i)); break;
			}
			this->x.predelete(lastbit(i));
		}
	};

	template<typename T1, typename T2, typename T3>
	struct cu_node_nontriv<T1,T2,T3> : cu_node_base<T1,T2,T3> {
		using B = cu_node_base<T1,T2,T3>;
		using B::B;
		using itype = typename B::itype;
		using type1 = typename B::type1;
		using type2 = typename B::type2;
		using type3 = typename B::type3;

		constexpr void predelete(itype i) {
			this->x.predelete(lastbit(i));
		}
	};

	template<typename T1, typename T2>
	struct cu_node_nontriv<T1,T2> : cu_node_base<T1,T2> {
		using B = cu_node_base<T1,T2>;
		using B::B;
		using itype = typename B::itype;
		using type1 = typename B::type1;
		using type2 = typename B::type2;

		constexpr void predelete(itype i) {
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
			alltrivialcopy<Ts...>::value,
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
				i(baseT::template index<decay<T>>::value) {}

		explicit constexpr cu_impl_base(const cu_impl_base &c) : v(c.i,c.v), i(c.i) {}
		explicit constexpr cu_impl_base(cu_impl_base &&c) : v(c.i,std::move(c.v)), i(c.i) {}

		template<typename... Ss>
		explicit constexpr cu_impl_base(const cu_impl_base<Ss...> &c)
				: v(cu_impl_base<Ss...>::baseT::template reindex<Ts...>(c.i),c.v),
				  i(cu_impl_base<Ss...>::baseT::template reindex<Ts...>(c.i)) {}

		template<typename... Ss>
		explicit constexpr cu_impl_base(cu_impl_base<Ss...> &&c)
				: v(cu_impl_base<Ss...>::baseT::template reindex<Ts...>(c.i),std::move(c.v)),
				  i(cu_impl_base<Ss...>::baseT::template reindex<Ts...>(c.i)) {}

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
			i = cu_impl_base<Ss...>::baseT::template reindex<Ts...>(c.i);
			v.assign(oldi,i,c.v);
			return *this;
		}
		template<typename... Ss>
		cu_impl_base &operator=(cu_impl_base<Ss...> &&c) {
			auto oldi = i;
			i = cu_impl_base<Ss...>::baseT::template reindex<Ts...>(c.i);
			v.assign(oldi,i,std::move(c.v));
			return *this;
		}

		template<typename F, typename... Ss>
		constexpr auto callfn(F &&f, Ss &&...args) const {
			return v.callfn(i,std::forward<F>(f),std::forward<Ss>(args)...);
		}
		template<typename F, typename... Ss>
		constexpr auto callfn(F &&f, Ss &&...args) {
			return v.callfn(i,std::forward<F>(f),std::forward<Ss>(args)...);
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
			alltrivialcopy<Ts...>::value,
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
