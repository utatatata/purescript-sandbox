module Main where

import Prelude
import Data.List
import Data.Show
import Data.Tuple
import Effect (Effect)
import Effect.Console (log, logShow)
import Dijkstra

main :: Effect Unit
main =
  let
    graph1 = fromFoldable
      [ (Tuple 0 1), (Tuple 0 2), (Tuple 0 3)
      , (Tuple 1 4), (Tuple 2 4), (Tuple 3 5)
      , (Tuple 4 5), (Tuple 3 5)
      ]
  in do
    log $ "Graph1: " <>  (show graph1)
    log $ "a path from 0 to 1: " <> (show (iddfs graph1 0 4))
    log $ "a path from 0 to 1: " <> (show (iddfs graph1 0 8))

