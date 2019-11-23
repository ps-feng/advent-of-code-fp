module Main where

import Prelude
import Effect (Effect)
import Benchmarks (runAllBenchmarks)

main :: Effect Unit
main = runAllBenchmarks
