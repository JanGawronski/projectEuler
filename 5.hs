module Main where

import qualified Data.Map.Strict as Map

unionMax :: (Ord k, Ord a) => Map.Map k a -> Map.Map k a -> Map.Map k a
unionMax = Map.unionWith max

howManyTimesDivides number divisor = if number `mod` divisor == 0 then 1 + howManyTimesDivides (number `div` divisor) divisor else 0

primeDivisors number = map (\x -> (x, howManyTimesDivides number x)) [2, 3, 5, 7, 11, 13, 17, 19]

answer = product $ map (uncurry (^)) $ Map.toList $ foldr1 unionMax $ map (Map.fromList . primeDivisors) [2 .. 20]

main = print answer