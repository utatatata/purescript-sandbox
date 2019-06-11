module Graph (Graph, fromFoldable) where

import Prelude
import Data.Foldable (class Foldable)
import Data.Foldable as Foldable
import Data.Set (Set(..))
import Data.Set as Set
import Graph.Edge (Edge(..))
import Graph.Edge as Edge
import Graph.Path (Path(..))
import Graph.Path as Path
import Graph.Vertex (Vertex(..))
import Graph.Vertex as Vertex


newtype Graph = Graph (Set Edge)

fromFoldable :: forall f. Foldable f => f Edge -> Graph
fromFoldable =
   Graph <<< Set.fromFoldable

