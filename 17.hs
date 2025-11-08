module Main where

upTo20 number = case number of
            0 -> ""
            1 -> "one"
            2 -> "two"
            3 -> "three"
            4 -> "four"
            5 -> "five"
            6 -> "six"
            7 -> "seven"
            8 -> "eight"
            9 -> "nine"
            10 -> "ten"
            11 -> "eleven"
            12 -> "twelve"
            13 -> "thirteen"
            14 -> "fourteen"
            15 -> "fifteen"
            16 -> "sixteen"
            17 -> "seventeen"
            18 -> "eighteen"
            19 -> "nineteen"
            20 -> "twenty"
            _ -> error "number out of range"

tens number = case number of
            2 -> "twenty"
            3 -> "thirty"
            4 -> "forty"
            5 -> "fifty"
            6 -> "sixty"
            7 -> "seventy"
            8 -> "eighty"
            9 -> "ninety"
            _ -> error "number out of range"

numberToWords number
    | number == 1000 = "one thousand"
    | number >= 100 = let hundreds = number `div` 100
                          rest = number `mod` 100
                      in upTo20 hundreds ++ " hundred" ++
                         (if rest /= 0 then " and " ++ numberToWords rest else "")
    | number >= 21 = let tensPart = number `div` 10
                         unitsPart = number `mod` 10
                     in tens tensPart ++
                        (if unitsPart /= 0 then " " ++ upTo20 unitsPart else "")
    | otherwise = upTo20 number


answer = sum $ map (length . filter (/= ' ') . numberToWords) [1..1000]

main = print answer