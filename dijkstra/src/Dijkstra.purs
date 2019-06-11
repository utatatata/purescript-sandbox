module Dijkstra where

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
import Data.Foldable hiding (null)
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

getVerteces :: Graph -> List Vertex
getVerteces graph = graph # concatMap (\(Tuple v1 v2) -> v1 : v2 : Nil)

getCurrentVerteces :: List Path -> List Vertex
getCurrentVerteces paths =
  paths
    # concatMap
        ( \path -> case head path of
          Just v -> v : Nil
          Nothing -> Nil
        )

getNext :: Graph -> Vertex -> Maybe (List Vertex)
getNext g v = case g # filter ((eq v) <<< fst)
    # map snd of
  Nil -> Nothing
  vs -> Just vs

getOneStepVerteces :: Graph -> Path -> Maybe (List Vertex)
getOneStepVerteces _ Nil = Nothing

getOneStepVerteces g (Cons current _) = getNext g current

getOneStepPaths :: Graph -> Path -> Maybe (List Path)
getOneStepPaths _ Nil = Nothing

getOneStepPaths graph path@(Cons currentVertex _) =
  getNext graph currentVertex
    <#> filter (flip notElem $ path)
    <#> map (\v -> v : path)

getNextPaths :: Graph -> List Path -> Maybe (List Path)
getNextPaths graph paths = case paths # concatMap (fromMaybe Nil <<< (getOneStepPaths graph)) of
  Nil -> Nothing
  paths -> Just paths

getLimitedPaths :: Depth -> Graph -> Vertex -> Maybe (List Path)
getLimitedPaths depth graph start = applyN ((=<<) (getNextPaths graph)) depth (Just ((start : Nil) : Nil))

reached :: Vertex -> Path -> Boolean
reached _ Nil = false

reached goal (Cons current _) = current == goal

data FailedToSearch
  = Unreached
  | Unreachable

instance showFailedToSearch :: Show FailedToSearch where
  show Unreached = "Unreached"
  show Unreachable = "Unreachable"

depthLimitedSearch :: Depth -> Graph -> Vertex -> Vertex -> Either FailedToSearch Path
depthLimitedSearch depth graph start goal = go depth ((start : Nil) : Nil)
  where
  go depth currentDepthPaths = case currentDepthPaths # find (reached goal) of
    Just path -> Right path
    Nothing -> if depth == 0
      then Left Unreached
      else case getNextPaths graph currentDepthPaths of
        Nothing -> Left Unreachable
        Just paths -> go (depth - 1) paths

iddfs :: Graph -> Vertex -> Vertex -> Maybe Path
iddfs graph start goal = go 0
  where
  go depth = case depthLimitedSearch depth graph start goal of
    Right path -> Just path
    Left Unreachable -> Nothing
    Left Unreached -> go (depth + 1)

dijkstra :: Graph -> Vertex -> Vertex -> Maybe Path
dijkstra graph start goal = go (getVerteces graph) ((start : Nil) : Nil)
  where
  go :: List Vertex -> List Path -> Maybe Path
  go unsearched currentPaths = case currentPaths # find (reached goal) of
    Just path -> Just path
    Nothing -> if null unsearched
      then Nothing
      else case getNextPaths graph currentPaths of
        Nothing -> Nothing
        Just paths ->
          go
            ( union unsearched (getCurrentVerteces currentPaths)
            ) paths
