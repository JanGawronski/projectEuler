module Main where

isPrime number = not $ any (\x -> number `mod` x == 0) [2..floor . sqrt $ (fromIntegral number :: Double)]

answer = filter isPrime [2..] !! 10000

main = print answer