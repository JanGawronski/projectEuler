module Main where

import Data.Set (fromList, size)

isPrime number = number >= 2 && not (any (\x -> number `mod` x == 0) [2..floor . sqrt $ (fromIntegral number :: Double)])

howManyTimesDivides number divisor = if number `mod` divisor == 0 then 1 + howManyTimesDivides (number `div` divisor) divisor else 0

primesUpTo100 = filter isPrime [1..100]

primeDivisors number = map (\x -> (x, howManyTimesDivides number x)) primesUpTo100

powers = fromList [map (\(x, y) -> (x, b * y)) $ primeDivisors a | a <- [2..100], b <- [2..100]]

answer = size powers 

main = print answer