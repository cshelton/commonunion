#include "../commonunion.h"
#include <iostream>

COMMONUNION(FooUnion,,foo,bar,baz)

struct A {
    int i;
    constexpr A(int initi) : i(initi) {}
    constexpr int foo(int a) const { return i+a; }
    constexpr int bar() const { return i*2; }	
	void baz() const { std::cout << bar() << std::endl; }
};
struct B {
    int i;
    constexpr B(int initi) : i(initi) {}
    constexpr int foo(int a) const { return i*a; }
    constexpr int bar() const { return i*i; }
	void baz() const { std::cout << bar() << std::endl; }
};
struct C {
    int i,j;
    constexpr C(int initi, int initj) : i(initi), j(initj) {}
    constexpr int foo(int a) const { return (i+(j*a))/j; }
    constexpr int bar() const { return 2*i/j; }
	void baz() const { std::cout << bar() << std::endl; }
};

template<typename T>
struct base {
	constexpr const T &getbase() const { return static_cast<const T &>(*this); }
	T &getbase() { return static_cast<const T &>(*this); }
	constexpr int doublefoo(int a) const { return 2*getbase().foo(a); }
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

COMMONUNION(Foo2Union,public base<cu_impl<Ts...>>,foo,bar,baz)

using namespace std;

int main(int argc, char **argv) {
	constexpr FooUnion<A,B> AorB{B{1}};
	constexpr FooUnion<C,A> CorA{C{3,2}};
	constexpr FooUnion<B,C,A> anyABC{C{7,2}};
	constexpr int res1 = AorB.foo(5);
	constexpr int res2 = CorA.foo(5);
	constexpr int res3 = anyABC.foo(5);
	cout << "5: " << res1 << endl;
	cout << "6: " << res2 << endl;
	cout << "8: " << res3 << endl;

	FooUnion<A,B,C> anyABC2{AorB};
	cout << "1: " << anyABC2.bar() << endl;
	anyABC2 = anyABC;
	cout << "7: " << anyABC2.bar() << endl;
	anyABC2 = CorA;
	cout << "3: " << anyABC2.bar() << endl;

	constexpr Foo2Union<A,B,C> u2(C{3,2});
	constexpr int res4 = u2.doublefoo(5);
	cout << "12: " << res4 << endl;

	cout << "--------------" << endl;
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
	cout << "3: "; u2.baz();
}
	
