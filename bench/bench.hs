module Main where

import qualified SetopBench

import Criterion.Main (bgroup, defaultMain)

main :: IO ()
main = defaultMain [ bgroup "Setop" SetopBench.benchmarks ]
