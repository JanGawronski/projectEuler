module Main where

factorial = product . enumFromTo 1

digitsInNumber number = if number `div` 10 > 0 then
                          (number `mod` 10):digitsInNumber (number `div` 10)
                        else
                          [number `mod` 10]

sumOfFactorialDigits = sum . map factorial . digitsInNumber

answer = sum $ filter (\x -> sumOfFactorialDigits x == x) [3..100000]

main = print answer