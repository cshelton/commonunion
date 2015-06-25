#include "../commonunion.h"
#include <iostream>

using namespace std;

COMMONUNION(FooUnion,,foo)

template<int I>
struct isval {
	constexpr int foo(int i) const { return i==I; }
};

template<int L, int H>
struct allvals {
	typedef FooUnion<typename allvals<L,(H+L)/2>::type,
				typename allvals<(H+L)/2+1,H>::type> type;
};

template<int L>
struct allvals<L,L> {
	typedef FooUnion<isval<L>> type;
};

constexpr int N = 257;

int main(int argc, char **argv) {
	typename allvals<1,N>::type x{isval<N/2>{}};
	cout << "size = " << sizeof(x) << endl;
	cout << "size = " << sizeof(uint_least8_t) << endl;
	cout << "size = " << sizeof(uint_least16_t) << endl;
	for(int i=1;i<=N;i++)
		cout << x.foo(i) << (i==N/2 ? '*' : ' ');
	cout << endl;
	x = isval<1>{};
	for(int i=1;i<=N;i++)
		cout << x.foo(i) << (i==1 ? '*' : ' ');
	cout << endl;
	x = isval<N>{};
	for(int i=1;i<=N;i++)
		cout << x.foo(i) << (i==N ? '*' : ' ');
	cout << endl;
}
