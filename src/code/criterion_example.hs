module Main where

import Criterion.Main

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main = defaultMain [
  bgroup "fib" [ bench "10" $ whnf fib 10
               , bench "20" $ whnf fib 35
               , bench "25" $ whnf fib 37
               ]
  ]
