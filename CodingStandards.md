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
