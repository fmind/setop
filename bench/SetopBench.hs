module SetopBench (benchmarks) where

import Setop

import Criterion

benchmarks :: [Benchmark]
benchmarks = [ bench "calculate 1" $ whnf calculate 1 ]
