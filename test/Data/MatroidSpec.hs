{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Data.MatroidSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

{-

This is a todo list of things that could be tested:

  -  rk is monotone increasing
  -  rk(\emptyset) = 0 (important special case)
  -  rk(X) <= |X| (subcardinal)
  -  rk(X+{x}) <= rk(X) + 1  (unit increasing)
  -  rk(A/\B) + rk(A\/B) <= rk(A) + rk(B) (submodular)
  
  -  indep X  &&  Y\subseteq X => indep Y (hereditary)
  -  indep \emptyset
  -  indep X && indep Y && |Y| > |X| => exists y\in Y: indep X+{y} (exchange property)

  -  basis X \subseteq X
  -  indep (basis X)
  -  y in X\(basis X) => not indep (basis X)+{y}
  
  -  cl is monotone
  -  cl(X) \subseteq E
  -  cl(cl(X)) == cl(X) (idempotence)
  -  e \in E\X, y\in cl(X+{e})\cl(X) => e \in cl(X+{y})

-}

spec :: Spec
spec = do
  describe "someFunction" $ do
    it "should work fine" $ do
      True `shouldBe` False
