module Main where

zipWithPad _  _  _ []     []     = []
zipWithPad _  pb f (x:xs) []     = f x pb : zipWithPad pb pb f xs []
zipWithPad pa _  f []     (y:ys) = f pa y : zipWithPad pa pa f [] ys
zipWithPad pa pb f (x:xs) (y:ys) = f x y  : zipWithPad pa pb f xs ys

add list1 list2 = dropWhile (==0) $ fst $ foldr (\c (acc, carry) -> ((c + carry) `mod` 10 : acc, (c + carry) `div` 10)) ([], 0) $ 0:0:0: reverse (zipWithPad 0 0 (+) (reverse list1) (reverse list2))

fib (current, previous) = (add current previous, current)

answer = snd $ head $ dropWhile ((< 1000) . length . fst . fst) $ zip (iterate fib ([1], [1])) [2..]

main = print answer