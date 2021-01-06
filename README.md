# matroid

This library provides typeclasses and basic functionality that revolves around the combinatorial structures
known as matroids.

## Language features

```
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
```

## Installation

Download and extract the sources to a directory of your liking. Then run
```
cabal install --lib matroid
```


## Usage

### Quick demo

For instance, download and extract the sources to a directory of your liking, then run
```
cabal repl
```

Then you get a prompt where you can start hacking. Let's see the groundset of the implementation
of the famous M(K_4) matroid, and mess around a bit:
```
*Data.Matroid> groundset $ mK 4
fromList [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
*Data.Matroid> import qualified Data.Set as S
*Data.Matroid S> rk (mK 4) $ S.fromList [(1,2),(1,3),(2,3)]
2
*Data.Matroid S> basis (mK 4) $ S.fromList [(1,2),(1,3),(2,3)]
fromList [(1,2),(1,3)]
*Data.Matroid S> cl (mK 4) $ S.fromList [(1,2),(1,3),(2,3)]
fromList [(1,2),(1,3),(2,3)]
*Data.Matroid S> rk (dual $ mK 4) $ S.fromList [(1,2),(1,3),(2,3)]
3
*Data.Matroid S> coRk (dual $ mK 4) $ S.fromList [(1,2),(1,3),(2,3)]
2
```
Of course, we got U_{4,2}, too:
```
*Data.Matroid S> uniform 4 2
uniformOn (fromList [1,2,3,4]) 2
```

### User-defined matroids

This library is most useful for implementing your own matroid types and make them an instance of ```Matroid m a```.
Here, the type ```m``` corresponds to a class of matroids, and the type ```a``` corresponds to the type of the matroid
elements. Thus a matroid always has a type of the form ```m a```. This makes sense because matroids arise in a lot of very
different situations, and it is important to be able to distinguish a graphic matroid where the edges are of type ```(Int,Int)```, 
from a linear matroid with the same edge type. In the graphic case, we would interpret the matroid elements as edges in a graph,
in the latter we might interpret the elements as vectors over a finite field.

In ```Data.Matroid.Ops``` we define a parametrized type ```AMatroid a``` that represents the notion of an abstract matroid,
which hides away the internals of the different matroid types from us, but also provides us with a way to accept any matroid
with elements of a given type as a parameter. You can convert any ```Matroid m a``` to ```AMatroid a``` with the ```abstract```
method. The instance definiton ```Matroid AMatroid a``` can be found in the ```Data.Matroid.Typeclass``` module. We would
like to mention that the default implementations of the various methods offered by the matroid typeclass ```Matroid m a``` 
are bounced back to ```AMatroid a``` where they are defined.

Writing a new ```Matroid m a``` instance is quite simple: you have to implement the ```groundset :: (m a) -> Set a``` routine
which returns the set of all elements of your matroid, and at least one of the following methods:

a) ```rk :: (m a) -> Set a -> Int``` which should return the rank of a set of matroid elements,
b) ```indep :: (m a) -> Set a -> Bool``` which should determine whether a set of matroid elements is independent,
c) ```basis :: (m a) -> Set a -> Set a``` which should determine an independent subset with maximal cardinality.

Once you have done this, all the other methods are provided by the library. It is advisable to also use one or more
of the test suites provided in the ```Test.Matroid``` module to verify that your implementation is indeed a matroid.
The ```matroidSuite``` also needs your matroid instance to be an instance of ```Show (m a)```.

Here is a minimal implementation of a rank 3 uniform matroid on the characters 'a' to 'g':
```
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import Data.Matroid
import qualified Data.Set as S
import Test.Hspec
import Test.QuickCheck
import Test.Matroid

data MyMatroid a = MyMatroid

instance Matroid MyMatroid Char where
    groundset m = S.fromList "abcdefg"
    rk MyMatroid x = min 3 $ length x

instance Show (MyMatroid Char) where
    show MyMatroid = "MyMatroid"

{- end of minimal implementation -}

m :: MyMatroid Char
m = MyMatroid

main :: IO ()
main = hspec (matroidSuite $ return m)
```
## How to run the library tests

Download and extract the sources to a directory of your liking. Then run
```
stack clean && stack build --test --coverage
```

## Browse the docs

Download and extract the sources to a directory of your liking. Then run
```
stack haddock --open .
```

## Contributing

Upload your changes and create a pull request on the [github repo](https://github.com/alb-i/h-matroid).