# Coding Standards and Rationales Behind Them

This document contains information about how we aim to organize the code in this project, together with to-do lists for common tasks.

## The Matroid Typeclass

The main anchor point of this library is the `Matroid` typeclass defined in the `Data.Matroid.Typeclass` module.

```
class (Ord a, Show a) => Matroid m a 
```

The class has two parameters, the second parameter `a` is the type of each of the matroid's elements; 
whereas the first parameter `m` represents the type of a matroid: Matroids arise naturally in a variety of
contexts and matroids as a structure have a variety of cryptomorphic axiom systems. Therefore it makes sense
to distinguish presentations of the same matroid with respect to different axiom systems, as well as to
be able to track certain classes of matroids that arise from special situations, as these classes usually
have important additional properties not shared by the abstract matroid structure.

### Using Notions from Different Cryptomorphic Axiom Systems

The most prominent ways to axiomatically define the matroid structure on the ground set `E` are

 -  the family of independent subsets of `E`
 -  the family of the maximal independent subsets (bases) of `E`
 -  the family of hyperplanes
 -  the rank function
 -  the closure operation
 -  the lattice of flats
 
Due to the cryptomorphic nature of matroids, every notion from any of these axiom systems may be expressed using the axiomatic notions of every other axiom system.
Also, some axiomatizations are more natural from a computational perspective and yield better performing results then others.

It is possible to derive matroids from any of its axiomatizations and therefore it is quite possible
to write an instance of the `Matroid` typeclass based on, say, a given matroid closure operator.
On the other hand, it would be preferable if a user of
the library could use default implementations of the notions from other axiom systems and be only left with
having to implement the operations needed for an axiom system of their liking. On the other hand, supporting all axiom systems takes too much resources.
Therefore, we currently support default definitions for the
typeclass members whenever either the rank function, an independence test, or a (flat) basis filter has been implemented.

We chose to fuse the default implementation of the `Matroid` typeclass functions into the abstract matroid type `AMatroid`, which comes in handy whenever we do no longer care about where the matroid is from, and also as the type that the canonical matroid constructions produce.
The `AMatroid` type is a record that allows to either store thunks to typeclass functions operation on a given matroid object or to defer to the default implementation of the underlying function with respect to the `Matroid` typeclass. This allows for all kinds of ad-hoc manipulation of matroids (a.k.a. constructions), as well as quick and dirty ways to test your implementation of a function against the default for performance and correctness (this especially goes both ways). Unfortunately, this comes at a price of higher manual maintenance. (There might be better ways to achieve something similar, if you know one, please reach out!)

### Adding a New Function To The `Matroid` Typeclass

Assume that we would like to add the function `f :: (m a) -> x` to the `Matroid m a` typeclass, including a default implementation in terms of other typeclass functions.
We have to do the following steps:

- We have to add a member field `w_f :: Maybe (x)` to the `WMatroid` record definition of the `AMatroid` data type that resides in `Data.Matroid.Ops` module.
- We have to update the `wrappedMatroid` record in `Data.Matroid.Ops` with the initialization `w_f = Nothing`.
- We have to add the member `f :: (m a) -> x` to the `Matroid m a` typeclass in the `Data.Matroid.Typclass` module and bounce its default implementation back to `AMatroid`:
```
     {- | This is the new matroid function -}
     f :: (m a) {- ^ the matroid -} -> x
     f m = f (abstract m) { w_f = Nothing }
```
- We have to fill the `w_f` member in the `wrapUp` function in `Data.Matroid.Typeclass` with the corresponding thunk by adding `w_f = Just $ f m`.
- We have to provide the default implementation of `f` in the `Matroid AMatroid a` instance definition in `Data.Matroid.Typeclass`, which shall be used whenever `w_f == Nothing` with:
```
f m = defaultsTo w_f m $ {- ... -}
```

where `{- ... -}` corresponds to the default implementation, which should be a new function in `Data.Matroid.Typeclass.Defaults`.