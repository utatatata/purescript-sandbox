module Graph.Vertex (Vertex(..), fromInt, getInt) where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple

newtype Vertex = Vertex Int
derive instance eqVertex :: Eq Vertex
derive instance ordVertex :: Ord Vertex

fromInt :: Int -> Vertex
fromInt = Vertex

getInt :: Vertex -> Int
getInt (Vertex i) = i