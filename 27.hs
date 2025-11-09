module Main where

import Data.IntSet (fromList, member)

isPrime number = number >= 2 && not (any (\x -> number `mod` x == 0) [2..floor . sqrt $ (fromIntegral number :: Double)])

rememberedPrimes = fromList $ takeWhile (<1000000) $ filter isPrime [2..]

isPrimemMem number = if number < 1000000 then number `member` rememberedPrimes else isPrime number 

primeFormula a b = length $ takeWhile isPrimemMem $ map (\x -> x*x + a * x + b) [0..]

answer = maximum [(primeFormula a b, a * b) | a <- [-1000..1000], b <- [a..1000]]

main = print answer