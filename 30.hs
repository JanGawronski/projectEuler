module Main where

sumOfPowerDigits 0 _ = 0
sumOfPowerDigits number power = (number `mod` 10) ^ power + sumOfPowerDigits (number `div` 10) power

isEqualToSOPD number power = number == sumOfPowerDigits number power

answer = sum $ filter (`isEqualToSOPD` 5) [2..sumOfPowerDigits 999999 5]

main = print answer