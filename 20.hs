module Main where

times list n = dropWhile (== 0) $ fst $ foldr (\c (acc, carry) -> ((n * c + carry) `mod` 10 : acc, (n * c + carry) `div` 10)) ([], 0) $ 0:0:list

answer = sum $ foldl times [1] [1..100]

main = print answer 