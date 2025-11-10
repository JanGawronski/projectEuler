module Main where

import Data.IntSet (fromList, member)

countDigits number = if number `div` 10 > 0 then 1 + countDigits (number `div` 10) else 1

removeRightDigit number = number `div` 10

removeLeftDigit number = number `mod` (10 ^ (countDigits number - 1))

rightRemovedNumber number = take (countDigits number) $ iterate removeRightDigit number

leftRemovedNumber number = take (countDigits number) $ iterate removeLeftDigit number

isPrime number = number >= 2 && not (any (\x -> number `mod` x == 0) [2..floor . sqrt $ (fromIntegral number :: Double)])

primesUpToMillion = fromList $ filter isPrime [1..1000000]

isPrimeMem number = number `member` primesUpToMillion 

isTruncatablePrime number = all isPrimeMem (rightRemovedNumber number ++ leftRemovedNumber number)

answer = sum $ filter isTruncatablePrime [10..1000000]

main = print answer