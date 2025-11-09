module Main where

import Data.Set (fromList, difference)

divisors number = if not (null smaller) && not (null bigger) && last smaller == head bigger
                 then smaller ++ tail bigger
                 else smaller ++ bigger
                where
                    smaller = filter (\x -> number `mod` x == 0) [1..floor $ sqrt (fromIntegral number :: Double)]
                    bigger  = map (number `div`) $ reverse smaller

abundantNumbers = filter (\x -> sum (divisors x) > 2 * x) [1..28123]

sumsOfAbundant = fromList [x + y | x <- abundantNumbers, y <- abundantNumbers, x >= y]


answer = sum $ difference (fromList [1..28123]) sumsOfAbundant

main = print answer