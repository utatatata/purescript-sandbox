module Graph.Edge (Edge, fromVertex, fromInt, start, end, swap, curry, uncurry) where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Graph.Vertex (Vertex(..))
import Graph.Vertex as Vertex

newtype Edge = Edge (Tuple Vertex Vertex)
derive instance eqEdge :: Eq Edge
derive instance ordEdge :: Ord Edge

fromVertex :: Vertex -> Vertex -> Edge
fromVertex v1 v2 = Edge $ Tuple v1 v2

fromInt :: Int -> Int -> Edge
fromInt a b =
  Edge $ Tuple (Vertex a) $ Vertex b

start :: Edge -> Vertex
start (Edge (Tuple e _)) = e

end :: Edge -> Vertex
end (Edge (Tuple _ e)) = e

swap :: Edge -> Edge
swap (Edge t) = Edge $ Tuple.swap t

curry :: forall a. (Edge -> a) -> Vertex -> Vertex -> a
curry f v1 v2 = f $ Edge $ Tuple v1 v2

uncurry :: forall a. (Vertex -> Vertex -> a) -> Edge -> a
uncurry f (Edge (Tuple v1 v2)) = f v1 v2
