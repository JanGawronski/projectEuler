module Main where

factorial = product . enumFromTo 1

binom n k = factorial n `div` (factorial k * factorial (n - k))

answer = binom 40 20

main = print answer