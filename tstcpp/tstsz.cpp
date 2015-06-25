#include "../commonunion.h"

COMMONUNION(FooUnion,,foo,bar)

struct A {
    int i;
    constexpr A(int initi) : i(initi) {}
    constexpr int foo(int a) const { return i+a; }
    constexpr int bar() const { return i*2; }
};
struct B {
    int i;
    constexpr B(int initi) : i(initi) {}
    constexpr int foo(int a) const { return i*a; }
    constexpr int bar() const { return i*i; }
};
struct C {
    int i,j;
    constexpr C(int initi, int initj) : i(initi), j(initj) {}
    constexpr int foo(int a) const { return (i+(j*a))/j; }
    constexpr int bar() const { return 2*i/j; }
};

struct Aplusbyte {
	A a;
	//unsigned char b;
	uint_least8_t b;
};

struct ABplusbyte {
	union {
		A a;
		B b;
	};
	//unsigned char i;
	uint_least8_t i;
};

template<std::size_t N>
struct Nbytes {
	unsigned char a[N];
};

struct leaf {
	bool res;
	constexpr bool ans() const { return res; }
};

template<typename T>
struct cons {
	bool cons;
	T cdr;
	constexpr bool ans() const { return cons && cdr.ans(); }
};

COMMONUNION(lst,,ans)

template<typename T>
auto mklst(bool v,const T &t) {
	return cons<T>{v,t};
}
template<typename T>
auto mklst(bool v,T &&t) {
	return cons<T>{v,std::move(t)};
}
	
template<std::size_t MAXN>
struct genlst {
	constexpr auto operator()(std::size_t l) const {
/*
		return CUEXPRSELECT(lst,
			(!l),
				leaf{true},
				mklst(true,genlst<MAXN-1>{}(l)));
*/
		return CUEXPRSELECT(lst,
			l==MAXN, (*this)(), genlst<MAXN-1>{}(l));
	}
	constexpr auto operator()() const {
		return mklst(true,genlst<MAXN-1>{}());
	}
};

template<>
struct genlst<1> {
	constexpr auto operator()(std::size_t l) const {
		return leaf{false};
	}
	constexpr auto operator()() const {
		return leaf{false};
	}
};

template<std::size_t MAXN>
constexpr auto almosttruelst(int l) {
	return genlst<MAXN>{}(l);
}

template<std::size_t MAXN>
constexpr auto almosttruelst() {
	return genlst<MAXN>{}();
}

template<std::size_t MAXN>
struct uptoNbytesT {
	constexpr auto operator()(std::size_t i) const {
		return CUEXPRSELECT(FooUnion,i==MAXN,
				Nbytes<MAXN>{}, uptoNbytesT<MAXN-1>{}(i));
	}
	constexpr auto operator()() const {
		return Nbytes<MAXN>{};
	}
};
template<>
struct uptoNbytesT<1> {
	constexpr auto operator()(std::size_t i) const {
		return Nbytes<1>{};
	}
	constexpr auto operator()() const {
		return Nbytes<1>{};
	}
};

template<std::size_t MAXN>
constexpr auto uptoNbytes(std::size_t i) {
	return uptoNbytesT<MAXN>{}(i);
}
template<std::size_t MAXN>
constexpr auto uptoNbytes() {
	return uptoNbytesT<MAXN>{}();
}

#include <iostream>
#include <typeinfo>
using namespace std;

constexpr std::size_t N = 60;

int main(int argc, char **argv) {
	cout << "A sz: " << sizeof(A) << endl;
	cout << "B sz: " << sizeof(B) << endl;
	cout << "C sz: " << sizeof(C) << endl;
	cout << "A+1 sz: " << sizeof(Aplusbyte) << endl;
	cout << "A/B+1 sz: " << sizeof(ABplusbyte) << endl;
	cout << "A/B sz: " << sizeof(FooUnion<A,B>) << endl;
	cout << "A/B/C sz: " << sizeof(FooUnion<A,B,C>) << endl;
	cout << "B/A/B sz: " << sizeof(FooUnion<B,A,B>) << endl;
	cout << "--------------" << endl;
	cout << "2 sz: " << sizeof(Nbytes<2>) << endl;
	cout << "3 sz: " << sizeof(Nbytes<3>) << endl;
	cout << "4 sz: " << sizeof(Nbytes<4>) << endl;
	cout << "5 sz: " << sizeof(Nbytes<5>) << endl;
	cout << "2/2 sz: " << sizeof(FooUnion<Nbytes<2>,Nbytes<2>>) << endl;
	cout << "2/3 sz: " << sizeof(FooUnion<Nbytes<2>,Nbytes<3>>) << endl;
	cout << "2/3/4 sz: " << sizeof(FooUnion<Nbytes<2>,Nbytes<3>,Nbytes<4>>) << endl;
	cout << "2/3/4/5 sz: " << sizeof(FooUnion<Nbytes<2>,Nbytes<3>,Nbytes<4>,Nbytes<5>>) << endl;

	cout << "--------------" << endl;

	auto lstfull=uptoNbytes<N>(); //almosttruelst<N>();
	cout << "size " << N << " lst: " << sizeof(decltype((lstfull))) << endl;
	//auto lst=almosttruelst<N>(1);
	auto lst=uptoNbytes<N>(1);
	cout << "up to " << N << " lst: " << sizeof(decltype((lst))) << endl;
	cout << typeid(lst).name() << endl;
}
	
