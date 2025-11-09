module Main where
import Data.List (maximumBy)
import Data.Function (on)

reciprocal number = reverse (dropWhile (==0) $ f [] 1 [])
                where
                    f list 0 past = list
                    f list n past
                            | n `div` number == 0 = f (0:list) (10 * n) past
                            | n `elem` past = list
                            | otherwise = f ((n `div` number):list) ((n `mod` number) * 10) (n:past)


answer = maximumBy (compare `on` (length . reciprocal)) [1..999]

main = print answer