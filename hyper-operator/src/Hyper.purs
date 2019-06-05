module Hyper (Exp(..), transform, transform1, transformCurrent) where

import Prelude
import Data.Int as Int
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.String.Common as String
import Data.TraversableWithIndex (traverseWithIndex)


data Exp
  = Hyper Int Exp Exp
  | Nat Int


instance showExp :: Show Exp where
  show (Hyper rank left right)= hyperToString rank (show left) $ show right
  show (Nat n) = Int.toStringAs Int.decimal n
  

hyperToString :: Int -> String -> String -> String
hyperToString rank left right =
  between "(" ")" $
    if rank == 0 then
      "succ " <> right
    else
      between left right $
        if rank == 1 then
          " + "
        else if rank == 2 then
          " * "
        else if rank == 3 then
          " ** "
        else
          (<>) " h" $ Int.toStringAs Int.decimal rank <> " "


between :: String -> String -> String -> String
between left right = (<>) left <<< flip (<>) right


transformCurrent :: Exp -> Maybe Exp
transformCurrent (Hyper rank left (Nat rightN))
  | rank == 0 = Just $ Nat $ rightN + 1
  | (Nat leftN) <- left
  , rank == 1
  , rightN == 0 = Just left
  | rank == 2
  , rightN == 0 = Just $ Nat 0
  | rank > 3
  , rightN == 0 = Just $ Nat 1
transformCurrent (Hyper rank left@(Nat _) (Nat rightN)) =
  Just $ Hyper (rank - 1) left $ Hyper rank left $ Nat $ rightN - 1
transformCurrent _ =
  Nothing


transform1 :: Exp -> Exp
transform1 exp@(Nat _) = exp
transform1 exp@(Hyper rank left right) =
  Maybe.maybe
    (Hyper rank (transform1 left) $ transform1 right)
    identity
    $ transformCurrent exp


transform :: Exp -> Exp
transform exp@(Nat _) = exp
transform exp =
  transform $ transform1 exp
