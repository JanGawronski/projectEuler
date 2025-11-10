module Main where

import Data.IntSet (fromList, member)

countDigits number = if number `div` 10 > 0 then 1 + countDigits (number `div` 10) else 1

circulateDigits number = (number `mod` 10) * (10 ^ (countDigits number - 1)) + (number `div` 10)

isPrime number = number >= 2 && not (any (\x -> number `mod` x == 0) [2..floor . sqrt $ (fromIntegral number :: Double)])

primesUpToMillion = fromList $ filter isPrime [1..1000000]

isPrimeMem number = number `member` primesUpToMillion 

isCircularPrime number = all isPrimeMem $ take (countDigits number) $ iterate circulateDigits number

answer = length $ filter isCircularPrime [1..1000000]

main = print answer