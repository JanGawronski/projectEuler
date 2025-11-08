module Main where
import Data.Foldable (maximumBy)
import Data.Function (on)

collatz number 
            | even number = number `div` 2
            | otherwise   = 3 * number + 1

collatzCount n = snd $ until ((== 1) . fst) (\(x, y) -> (collatz x, succ y)) (n, 1)

answer = maximumBy (compare `on` collatzCount) [1..1000000]

main = print answer