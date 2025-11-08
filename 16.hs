module Main where

start = [1]

times2 :: [Int] -> [Int]
times2 list = dropWhile (== 0) $ fst $ foldr (\c (acc, carry) -> ((2 * c + carry) `mod` 10 : acc, (2 * c + carry) `div` 10)) ([], 0) $ 0:list

answer = sum $ iterate times2 start !! 1000

main = print answer 