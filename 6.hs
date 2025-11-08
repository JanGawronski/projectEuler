module Main where

squareOfSums number = let s = number * (number + 1) `div` 2 in s * s

sumOfSquares number = number * (number + 1) * (2 * number + 1) `div` 6

answer = squareOfSums 100 - sumOfSquares 100

main = print answer