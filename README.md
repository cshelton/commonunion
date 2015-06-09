commonunion
===========
Union of types with common member functions, suitable for use in constexpr, C++14 only

Virtual member functions are terrific for implementing dynamic dispatch.  If you need dynamic dispatch, you should use the compile-supplied solution and not build your own.  

*However*, C++ does not directly support two things:
* virtual template functions
* use of dynamic dispatch in constexpr functions

This library provides these two things, but loses the universalism of virtual classes.  In particular, the set of possible classes for a particular variable must be known at compile time (and therefore must be finite).

Overview
--------

Essentially, it allows you to define a (template) class that can store any one of a number of types (the types in the template arguments).  When you define the type, you also specify the names of any member functions all types that will be used by the class must contain.

For example, if you defined the type `FooUnion` in this way and said that it had a member function foo, `FooUnion<A,B>` is a type that can hold either an `A` or a `B`.  `FooUnion<A,B>` has a member function foo that calls either `A::foo` or `B::foo`, depending on which type is currently stored.  `FooUnion<A,B>::foo` is templated to work with any argument list, constness, and return type.

`FooUnion<A,B,C>` can hold an `A`, a `B`, or a `C`.  Any number of template arguments can be specified.  `FooUnion<A,FooUnion<B,C>>` is the same as `FooUnion<A,B,C>`.  `FooUnion<B,A,C>` is techinically a different type, but can be used almost interchangably with the previous two.

If the return types from `A::foo`, `B::foo`, and `C::foo` are the same, this is all fine.  If they are convertible (in the `std::common_type` sense), this is also fine.  If they return different types, then `FooUnion<...>` can be set up to so that the return type of `FooUnion<...>::foo` be a common union itself (of the return types of `A::foo`, `B:foo`, ...).


How to use
----------

This is best illustrated by an example.  Consider you have three classes.  They differ in how `foo` treats its argument and in what they store.

```c++
class A {
	int i;
	constexpr A(int initi) : i(initi) {}
	constexpr int foo(int a) const { return i+a; }
};
class B {
	int i;
	constexpr B(int initi) : i(initi) {}
	constexpr int foo(int a) const { return i*a; }
};
class C {
	int i,j;
	constexpr C(int initi, int initj) : i(initi), j(initj) {}
	constexpr int foo(int a) const { return (i+(j*a))/j; }
};
```

you can now write
```c++
COMMONUNION(FooUnion,foo)
```
which declares a (variadic template) type `FooUnion` that can hold any type (`A`, `B`, and `C` above are three examples) that declares a method called `foo`.

A simple example of use:

```c++
constexpr FooUnion<A,B> AorB{B{1}};
constexpr FooUnion<C,A> CorA{C{3,2}};
constexpr FooUnion<B,C,A> anyABC{C{1,2}};
constexpr int res1 = AorB.foo(5);
constexpr int res2 = CorA.foo(5);
constexpr int res3 = anyABC.foo(5);

FooUnion<A,B,C> anyABC2{AorB};
anyABC2 = anyABC;
anyABC2 = CorA;
```
Note that because the types `A`, `B`, and `C` are trivial (they have trivial constructors and destructors), `FooUnion` with arguments of these types can be used in `constexpr` expressions.
Assignments (as shown toward the end), can be made provided that the set of types only grows (you cannot assign a `FooUnion<A,B,C>` to a `FooUnion<A,C>`.

These type are perhaps most helpful in expressions like
```c++
constexpr FooUnion<A,C> factory(int i, int j) {
	return j==1 ? A{i}, C{i,j};
}
```

As the types involved in such expressions can be difficult to write down, a simple macro
can be used to generate a conditional expression such as the one above with the
correct types.  The `factory` above would be rewritten
```c++
constexpr auto factory(int i, int j) {
	return CUEXPRSELECT(FooUnion,
			(j==1),
				A{i},
				C{i,j});
}
```
This is particularly useful when the subexpressions generate common-unions themselves.
Here is a (particularly contrived) example:
```c++
constexpr auto addormult(int i, bool usemult) {
	return CUEXPRSELECT(FooUnion,
			usemult,
				B{i},
				A{i});
}
constexpr auto newfactory(int i, int j, int k) {
	return CUEXPRSELECT(FooUnion,
			j==k,
				addormult(i,i==j),
				factory(i,j));
}
```
	




