module Main where
import Data.Function (on)
import Data.List (maximumBy)


coprime a b = gcd a b == 1

pythogoreanTriples number = [(m^2 - n^2, 2 * m * n, m^2 + n^2) | m <- [1..number], n <- [1..m - 1], even m /= even n, coprime n m, m^2 + 2 * m * n <= number]

pythogoreanSums number = map (\(a, b, c) -> a + b + c) $ pythogoreanTriples number

calculatedSums = pythogoreanSums 1000

solutions number = length $ filter (\x -> number `mod` x == 0) calculatedSums

answer = maximumBy (compare `on` solutions) [1..1000]

main = print answer