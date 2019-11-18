module Test.Main where

import Prelude
import Test.Day1 as Day1
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  Day1.main
  log "You should add some tests."
