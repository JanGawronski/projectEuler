module Main where

divisors number = if not (null smaller) && not (null bigger) && last smaller == head bigger
                 then smaller ++ tail bigger
                 else smaller ++ bigger
                where
                    smaller = filter (\x -> number `mod` x == 0) [1..floor $ sqrt (fromIntegral number :: Double)]
                    bigger  = map (number `div`) $ reverse smaller

amicablePartner number = if candidate /= number && sum (divisors candidate) - candidate == number then Just candidate else Nothing
                        where
                            candidate = sum (divisors number) - number

answer = sum [x + y | x <- [1..10000], Just y <- [amicablePartner x], x > y]

main = print answer 