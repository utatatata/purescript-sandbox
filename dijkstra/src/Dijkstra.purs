module Dijkstra  where

import Prelude
-- import Data.Foldable (class Foldable)
-- import Data.Foldable as Foldable
-- import Data.List (List(..), (:))
-- import Data.List as List
-- import Data.Maybe (Maybe(..))
-- import Data.Maybe as Maybe
-- import Data.Tuple (Tuple(..))
-- import Data.Tuple as Tuple
import Data.Either
import Data.Foldable
import Data.Function
import Data.List
import Data.Maybe
import Data.Traversable
import Data.Tuple
import Data.Unfoldable hiding (range, fromMaybe)


type Vertex
  = Int

type Edge
  = Tuple Int Int

type Graph
  = List Edge

type Path
  = List Vertex

type Depth
  = Int

getNext :: Graph -> Vertex -> Maybe (List Vertex)
getNext g v =
  case g # filter ((eq v) <<< fst)
         # map snd of
    Nil -> Nothing
    vs -> Just vs
  

getOneStepPaths :: Graph -> Path -> Maybe (List Path)
getOneStepPaths _ Nil = Nothing
getOneStepPaths _ (Cons _ Nil) = Nothing
getOneStepPaths graph path@(Cons currentVertex (Cons previousVertex _)) =
  getNext graph currentVertex
    <#> filter (flip elem $ path)
    <#> map (\v -> v:path)

getNextPaths :: Graph -> List Path -> Maybe (List Path)
getNextPaths graph paths =
  case paths # concatMap (fromMaybe Nil <<< (getOneStepPaths graph)) of
    Nil -> Nothing
    paths -> Just paths


getLimitedPaths :: Depth -> Graph -> Vertex -> Maybe (List Path)
getLimitedPaths depth graph start =
  -- applyN (getNextPaths graph) depth ((start:Nil):Nil)
  applyN ((=<<) (getNextPaths graph)) depth (Just ((start:Nil):Nil))


reached :: Vertex -> Path -> Boolean
reached _ Nil = false
reached goal (Cons current _) = current == goal

data FailedToSearch
  = Unreached
  | Nowhere

depthLimitedSearch :: Depth -> Graph -> Vertex -> Vertex -> Either FailedToSearch Path
depthLimitedSearch depth graph start goal =
  go depth ((start:Nil):Nil)
  where
    go depth currentDepthPaths =
      case currentDepthPaths # find (reached goal) of
        Just path -> Right path
        Nothing ->
          if depth == 0
          then Left Unreached
          else case getNextPaths graph currentDepthPaths of
            Nothing -> Left Nowhere
            Just paths -> go (depth - 1) paths

iddfs :: Graph -> Vertex -> Vertex -> Maybe Path
iddfs graph start goal =
  go 0
  where
    go depth =
      case depthLimitedSearch depth graph start goal of
        Right path -> Just path
        Left Nowhere -> Nothing
        Left Unreached -> go (depth + 1)
