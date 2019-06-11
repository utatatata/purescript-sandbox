module Graph.Trail (Trail, empty, singleton, push, append) where

import Prelude
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Graph.Edge (Edge)
import Graph.Edge as Edge

newtype Trail = Trail (List Edge)

empty :: Trail
empty = Trail Nil

singleton :: Edge -> Trail
singleton = Trail <<< List.singleton

push :: Edge -> Trail -> Maybe Trail
push e (Trail es) =
  if List.notElem e es then
    Just $ Trail $ List.snoc es e
  else
   Nothing

append :: Trail -> Trail -> Maybe Trail
append (Trail es1) (Trail es2) =
  if List.null $ List.intersect es1 es2 then
    Just $ Trail $ List.union es1 es2
  else
    Nothing
