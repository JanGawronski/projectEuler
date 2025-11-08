module Main where


answer = head [a * b * c | a <- [2..999], b <- [2..a], let c = 1000 - a - b, a^2 + b^2 == c^2]

main = print answer