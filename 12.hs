module Main where

import Data.List (inits)

divisorsCount :: Integral a => a -> Int
divisorsCount number = (*2) $ length $ filter (\x -> number `mod` x == 0) [1 .. floor $ sqrt (fromIntegral number :: Double)]

triangleNumbers = scanl1 (+) [1..]

answer = head $ filter ((>500) . divisorsCount) triangleNumbers

main = print answer