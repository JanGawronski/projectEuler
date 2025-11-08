module Main where

bigNumber = 600851475143

ceilingSqrt = succ . ceiling . sqrt . fromIntegral

descendingFromSqrt number = [top, top - 1 .. 2] where top = ceilingSqrt number

isPrime :: Int -> Bool
isPrime number = not . any (\x -> number `mod` x == 0) $ descendingFromSqrt number

primes = filter isPrime $ descendingFromSqrt bigNumber

answer = head $ filter (\x -> bigNumber `mod` x == 0) primes

main = print answer