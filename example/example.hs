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

m :: MyMatroid Char
m = MyMatroid

main :: IO ()
main = hspec (matroidSuite $ return m)