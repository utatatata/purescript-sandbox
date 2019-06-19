module Trs where

import Prelude
import Data.List

data Term a
    = Nil
    | Cons a (Term a)
    | Concat (Term a) (Term a)

derive instance eqTermInt :: Eq (Term Int)
instance showTermInt :: Show (Term Int) where
    show Nil = "Nil"
    show (Cons x y) = "(Cons " <> show x <> " " <> show y <> ")"
    show (Concat x y) = "(Concat " <> show x <> " " <> show y <> ")"

rewrite :: forall a. Term a -> Term a
rewrite (Cons x y) = Cons x $ rewrite y
rewrite (Concat Nil term) = rewrite term
rewrite (Concat (Cons head tail) term) =  rewrite $ Cons head (Concat tail term)
rewrite term = term
