module Main where

repeated = concatMap (replicate 4) [2,4..1000]

shifted = scanl (+) 1 repeated

answer = sum shifted

main = print answer