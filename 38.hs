module Main where

import Data.Bits (shiftL, (.|.), (.&.), xor, complement)

countDigits number = if number `div` 10 > 0 then 1 + countDigits (number `div` 10) else 1

isPandigital number = go number 0
                    where
                        go :: Int -> Int -> Bool
                        go n mask 
                            | number == 0 = False
                            | n == 0 = (mask + 2) == 2 `shiftL` countDigits number
                            | mask .&. (1 `shiftL` (n `mod` 10)) /= 0 = False
                            | otherwise = go (n `div` 10) (mask .|. (1 `shiftL` (n `mod` 10)))

concatNumbers :: [Int] -> Int
concatNumbers = foldl (\acc x -> acc * (10 ^ countDigits x) + x) 0

multiply n1 n2 = concatNumbers $ map (n1*) [1..n2]

answer = head $ filter isPandigital [multiply a b | a <- [9999,9998..1], b <- [1..9]]

main = print answer