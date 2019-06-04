module Hyper (Exp(..), transform, transform1, transformCurrent) where

import Prelude

import Data.Int as Int
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.String.Common as String
import Data.TraversableWithIndex (traverseWithIndex)


data Exp
  = Hyper Exp Int Exp
  | Nat Int


instance showExp :: Show Exp where
  show (Hyper left rank right)= "(" <> hyperToString (show left) rank (show right) <> ")"
  show (Nat n) = Int.toStringAs Int.decimal n
  

hyperToString :: String -> Int -> String -> String
hyperToString left rank right =
  if rank == 0 then
    "succ " <> right
  else
    (<>) left <<< flip (<>) right $
      if rank == 1 then
        " + "
      else if rank == 2 then
        " * "
      else if rank == 3 then
        " ** "
      else
        (<>) " h" $ Int.toStringAs Int.decimal rank


transformCurrent :: Exp -> Maybe Exp
transformCurrent (Nat _) =
  Nothing
transformCurrent (Hyper left rank (Nat rightN))
  | rank == 0 = Just $ Nat $ rightN + 1
  | (Nat leftN) <- left
  , rank == 1
  , rightN == 0 = Just left
  | rank == 2
  , rightN == 0 = Just $ Nat 0
  | rank > 3
  , rightN == 0 = Just $ Nat 1
transformCurrent (Hyper left@(Nat _) rank (Nat rightN)) =
  Just $ Hyper left (rank - 1) $ Hyper left rank $ Nat $ rightN - 1
transformCurrent (Hyper _ _ _) =
  Nothing


transform1 :: Exp -> Exp
transform1 exp@(Nat _) = exp
transform1 exp@(Hyper left rank right) =
  Maybe.maybe
    (Hyper (transform1 left) rank $ transform1 right)
    identity
    $ transformCurrent exp


transform :: Exp -> Exp
transform exp@(Nat _) = exp
transform exp =
  transform $ transform1 exp
