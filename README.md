commonunion
===========
Union of types with common member functions, suitable for use in constexpr, C++14 only

Virtual member functions are terrific for implementing dynamic dispatch.  If you need dynamic dispatch, you should use the compile-supplied solution and not build your own.  

*However*, C++ does not support two things:
* Virtual template functions
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

Best illustrated by an example.  Consider you have three classes.  They differ in how `foo` treats its argument and in what they store.

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
