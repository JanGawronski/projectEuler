module Main where

import Data.IntSet (fromList, toList)

import Data.Bits (shiftL, (.|.), (.&.), xor, complement)

mark :: Int -> Int
mark = (`div` 2) . go 0
        where
            go mask 0 = mask
            go mask n 
                | mask .&. (1 `shiftL` (n `mod` 10)) /= 0 = 0
                | otherwise = go (mask .|. (1 `shiftL` (n `mod` 10))) (n `div` 10)


goodToMark :: Int -> Bool
goodToMark = go 0
        where
            go :: Int -> Int -> Bool
            go mask 0 = True
            go mask n
                | n `mod` 10 == 0 = False
                | mask .&. (1 `shiftL` (n `mod` 10)) /= 0 = False
                | otherwise = go (mask .|. (1 `shiftL` (n `mod` 10))) (n `div` 10)


xor3 a b c = (==511) $ (a `xor` b `xor` c) .&. complement (a .&. b .&. c)


answer = sum $ toList $ fromList [a*b | a <- [0..10000], goodToMark a, b <- [a..10000], goodToMark b, goodToMark (a*b), xor3 (mark a) (mark b) (mark (a*b))]

main = print answer