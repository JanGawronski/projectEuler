module Main where
import Data.List (intersect, nub)

areFractionsEqual (n1, d1) (n2, d2) = n1 * d2 == n2 * d1

digitsInNumber number = nub $ if number `div` 10 > 0 then
                                (number `mod` 10):digitsInNumber (number `div` 10)
                              else
                                [number `mod` 10]

removeDigit number digit = if number `mod` 10 == digit then
                            number `div` 10
                           else
                            removeDigit (number `div` 10) digit * 10 + number `mod` 10

cancelDigit (n, d) = go (digitsInNumber n `intersect` digitsInNumber d)
                    where
                        go [] = []
                        go (x:xs) = (removeDigit n x, removeDigit d x):go xs
                        

isFractionNonTrivial (n, d) = any (\(x, y) -> x /= 0 && y /= 0 && x * 10 /= n && y * 10 /= d && areFractionsEqual (n, d) (x, y)) (cancelDigit (n, d))


fractions = [(n, d) | d <- [1..99], n <- [1..d - 1], isFractionNonTrivial (n, d)]


isPrime number = number >= 2 && not (any (\x -> number `mod` x == 0) [2..floor . sqrt $ (fromIntegral number :: Double)])

howManyTimesDivides number divisor = if number `mod` divisor == 0 then 1 + howManyTimesDivides (number `div` divisor) divisor else 0

primesUpTo100 = filter isPrime [1..100]

primeDivisors number = map (\x -> (x, howManyTimesDivides number x)) primesUpTo100

fraction = (product $ map fst fractions, product $ map snd fractions)

answer = product $ map (uncurry (^)) $ zipWith (\(p1, d1) (p2, d2) -> (p1, d2 - min d1 d2)) (primeDivisors $ fst fraction) (primeDivisors $ snd fraction)

main = print answer