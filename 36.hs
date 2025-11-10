module Main where

countDigits number base = if number `div` base > 0 then 1 + countDigits (number `div` base) base else 1

reverseNumber number base = if number `div` base /= 0 then (number `mod` base) * (base ^ (countDigits number base - 1)) + reverseNumber (number `div` base) base else number `mod` base

isPalindromic number base = go number (reverseNumber number base)
                    where
                        go 0 0 = True
                        go 0 _ = False
                        go _ 0 = False
                        go n1 n2 = (n1 `mod` base == n2 `mod` base) && go (n1 `div` base) (n2 `div` base)



answer = sum $ filter (\x -> isPalindromic x 10 && isPalindromic x 2) [1..999999]

main = print answer