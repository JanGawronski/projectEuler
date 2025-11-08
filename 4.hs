module Main where

products = [a * b | a <- [999, 998 .. 100], b <- [999, 998 .. a]]

isPalindrom number = str == reverse str where str = show number

answer = maximum $ filter isPalindrom products

main = print answer