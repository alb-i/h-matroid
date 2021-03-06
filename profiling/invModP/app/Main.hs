{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Fields.Zp
import Control.DeepSeq
import System.Environment

run_test :: String -> IO ()
run_test "strict" = do    
    let measureExpression = map (invModPstrict 141650939) [1..100000000]
     in measureExpression `deepseq` putStrLn "strict invModP done."
run_test "lazy" = do
    let measureExpression = map (invModPlazy 141650939) [1..100000000]
     in measureExpression `deepseq` putStrLn "lazy invModP done."
run_test "matroid" = do
        let measureExpression = map (invModP 141650939) [1..100000000]
         in measureExpression `deepseq` putStrLn "lazy invModP done."
        

main :: IO ()
main = do
    [x] <- getArgs
    run_test x
    

{-| lazily determine the multiplicative inverse in Z/Zp (of x) -}
invModPlazy :: Integer {- ^ characteristic p -} -> Integer {- ^ element to invert, != 0 mod p -} -> Integer
invModPlazy p x = euclidStep p 0 x 1
  where euclidStep r0 t0 r1 t1 -- see: https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Modular_integers
           | r1 == 0 && r0 > 1 = error "Input element not invertible!"
           | r1 == 0 && t0 < 0 = t0 + p
           | r1 == 0 = t0
           | otherwise = let (quotient,remainder) = r0 `divMod` r1
                         in euclidStep r1 t1 remainder (t0 - quotient*t1)
                         
{-| determine the multiplicative inverse in Z/Zp (of x) -}
invModPstrict :: Integer {- ^ characteristic p -} -> Integer {- ^ element to invert, != 0 mod p -} -> Integer
invModPstrict p x = euclidStep p 0 x 1
  where euclidStep r0 t0 !r1 !t1 -- see: https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Modular_integers
           | r1 == 0 && r0 > 1 = error "Input element not invertible!"
           | r1 == 0 && t0 < 0 = t0 + p
           | r1 == 0 = t0
           | otherwise = let (quotient,remainder) = r0 `divMod` r1
                         in euclidStep r1 t1 remainder (t0 - quotient*t1)
