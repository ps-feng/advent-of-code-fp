module Benchmarks
  -- ( runAllBenchmarks )
where

-- import Prelude

-- import Benchotron.Core (Benchmark, benchFn, mkBenchmark)
-- import Benchotron.UI.Console (runSuite)
-- import Effect (Effect)
-- import Test.QuickCheck.Gen (chooseInt)

-- import Day6 (makeCoord, positionsAtDistance, newPositionsAtDistance)

-- benchmarkForDay6 :: Benchmark
-- benchmarkForDay6 = mkBenchmark
--   { slug: "position_to_distance"
--   , title: "Calculating the positions at some distance from (0, 0)"
--   , sizes: [0, 1, 3, 50, 100, 500]
--   , sizeInterpretation: "Distance"
--   , inputsPerSize: 1
--   , gen: \n -> chooseInt n n
--   , functions: [ benchFn "Pin" (positionsAtDistance (makeCoord 0 0))
--                , benchFn "Lelle" (newPositionsAtDistance (makeCoord 0 0)) 
--                ]
--   }

-- runAllBenchmarks :: Effect Unit
-- runAllBenchmarks = runSuite [benchmarkForDay6]
