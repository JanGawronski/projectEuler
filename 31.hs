module Main where

import Data.List (tails)

coins = [200,100,50,20,10,5,2,1]

ways list number 
                | number == 200 = 1
                | number >  200 = 0
                | otherwise = sum (map (\x -> ways x (number + head x)) $ init $ tails list)

answer = ways coins 0

main = print answer