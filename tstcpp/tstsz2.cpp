#include "../commonunion.h"
#include <iostream>
#include <typeinfo>

using namespace std;

COMMONUNION(FooUnion,,foo)

#define defstruct(name) struct name { int foo() { cout << #name << endl; return 0; } };

defstruct(A) defstruct(B) defstruct(C) defstruct(D) defstruct(E) defstruct(F) defstruct(G) defstruct(H)
defstruct(I) defstruct(J) defstruct(K) defstruct(L) defstruct(M) defstruct(N) defstruct(O) defstruct(P)
defstruct(Q) defstruct(R) defstruct(S) defstruct(T) defstruct(U) defstruct(V) defstruct(W) defstruct(X)
defstruct(Y) defstruct(Z)


int main(int argc, char **argv) {
	FooUnion<A,B,C,D,E,F,G,H,I,J,K,L,M,N,O> fu{B{}};
	fu.foo();
}
	
