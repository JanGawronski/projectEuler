module Main where

arithmeticSeries start limit step =
      let n = (limit - start) `div` step + 1
          last = start + (n - 1) * step
      in (start + last) * n `div` 2

answer = arithmeticSeries 0 999 3 + arithmeticSeries 0 999 5 - arithmeticSeries 0 999 15

main :: IO ()
main = print answer
