module Graph.Path (Path, empty, singleton, push, append) where

import Prelude
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Graph.Vertex (Vertex(..))
import Graph.Vertex as Vertex

newtype Path = Path (List Vertex)

empty :: Path
empty = Path Nil

singleton :: Vertex -> Path
singleton = Path <<< List.singleton

push :: Vertex -> Path -> Maybe Path
push v (Path vs) =
  if List.notElem v vs then
    Just $ Path $ List.snoc vs v
  else
   Nothing

append :: Path -> Path -> Maybe Path
append (Path vs1) (Path vs2) =
  if List.null $ List.intersect vs1 vs2 then
    Just $ Path $ List.union vs1 vs2
  else
    Nothing
