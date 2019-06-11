module Graph.Walk (Walk, empty, singleton, push, append) where

import Prelude
import Data.List (List(..))
import Data.List as List
import Graph.Vertex (Vertex(..))
import Graph.Vertex as Vertex

newtype Walk = Walk (List Vertex)

empty :: Walk
empty = Walk Nil

singleton :: Vertex -> Walk
singleton = Walk <<< List.singleton

push :: Vertex -> Walk -> Walk
push v (Walk vs) = Walk $ List.snoc vs v

append :: Walk -> Walk -> Walk
append (Walk vs1) (Walk vs2) = Walk $ List.union vs1 vs2
