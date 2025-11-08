module Main where

isPrime number = not $ any (\x -> number `mod` x == 0) [2..floor . sqrt $ (fromIntegral number :: Double)]

answer = sum $ takeWhile (<2000000) $ filter isPrime [2..]

main = print answer