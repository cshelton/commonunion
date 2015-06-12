#include "commonunion.h"
#include <typeinfo>

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

#include <iostream>
using namespace std;

int main(int argc, char **argv) {
	FooUnion<A,B> ab{A{1}};
	cout << commonunion::istemplateinst<commonunion::cu_node_base,decltype(ab.v)>::value << endl;
	cout << typeid(ab.v).name() << endl;
}
	
