module Main where

monthsLengths year
                 | year `mod` 400 == 0 = leapYear
                 | year `mod` 100 == 0 = normalYear
                 | year `mod`   4 == 0 = leapYear
                 | otherwise           = normalYear
                 where 
                    leapYear   = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
                    normalYear = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

yearlyWeekdayShift = map (`mod` 7) . scanl1 (+) . monthsLengths

concatenateShift list1 list2 = list1 ++ map ((`mod` 7) . (+ last list1)) list2

answer = length $ filter (==0) $ foldl concatenateShift [2] $ map yearlyWeekdayShift [1901..2000]

main = print answer