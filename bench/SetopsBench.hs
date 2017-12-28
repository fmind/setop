module SetopsBench (benchmarks) where

import Setops

import Criterion

benchmarks :: [Benchmark]
benchmarks = [ bench "calculate 1" $ whnf calculate 1 ]
