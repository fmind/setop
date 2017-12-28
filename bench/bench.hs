module Main where

import qualified SetopsBench

import Criterion.Main (bgroup, defaultMain)

main :: IO ()
main = defaultMain [ bgroup "Setops" SetopsBench.benchmarks ]
