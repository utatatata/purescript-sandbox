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
    graph1 =
      fromFoldable
        [ (Tuple 0 1)
        , (Tuple 0 2)
        , (Tuple 0 3)
        , (Tuple 1 4)
        , (Tuple 2 4)
        , (Tuple 3 5)
        , (Tuple 4 5)
        , (Tuple 3 5)
        , (Tuple 8 9)
        ]
  in
    do
      log $ "Graph1: " <> (show graph1)
      log "Limited depth search:"
      log $ "a path from 0 to 4, depth limit is 2: " <> (show (depthLimitedSearch 2 graph1 0 4))
      log $ "a path from 0 to 4, depth limit is 1: " <> (show (depthLimitedSearch 1 graph1 0 4))
      log $ "a path from 0 to 8, depth limit is 10: " <> (show (depthLimitedSearch 10 graph1 0 8))
      log "IDDFS:"
      log $ "a path from 0 to 4: " <> (show (iddfs graph1 0 4))
      log $ "a path from 0 to 5: " <> (show (iddfs graph1 0 5))
      log $ "a path from 0 to 8: " <> (show (iddfs graph1 0 8))
      log "Dijkstra:"
      log $ "a path from 0 to 4: " <> (show (dijkstra graph1 0 4))
      log $ "a path from 0 to 5: " <> (show (dijkstra graph1 0 5))
      log $ "a path from 0 to 8: " <> (show (dijkstra graph1 0 8))
