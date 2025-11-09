module Main where

import Data.List (group)



factorial = product . enumFromTo 1

sumOfFactorials number upperLimit
                | factorial upperLimit > number  = sumOfFactorials number (upperLimit - 1) 
                | factorial upperLimit == number = [upperLimit]
                | factorial upperLimit < number  = upperLimit: sumOfFactorials (number - factorial upperLimit) upperLimit



positions = map length $ group $ sumOfFactorials 999999 10

answer = fst $ foldl (\(added, rest) c -> (added ++ [rest !! c], filter (/= (rest !! c)) rest)) ([], ['0'..'9']) (positions ++ [0])


main = print answer
