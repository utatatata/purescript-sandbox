module Trs where

import Prelude (class Show, class Eq, show, ($), (<$>), (<>))
import Data.Maybe (Maybe(..))

data Term a
    = Nil
    | Cons a (Term a)
    | Concat (Term a) (Term a)
    | Snoc (Term a) a

derive instance eqTermInt :: Eq (Term Int)
instance showTermInt :: Show (Term Int) where
    show Nil = "Nil"
    show (Cons x y) = "(Cons " <> show x <> " " <> show y <> ")"
    show (Concat x y) = "(Concat " <> show x <> " " <> show y <> ")"
    show (Snoc x y) = "(Snoc " <> show x <> " " <> show y <> ")"

rewrite :: forall a. Term a -> Maybe (Term a)
rewrite (Cons x y) = (Cons x) <$> (rewrite y)
rewrite (Concat Nil term) = rewrite term
rewrite (Concat (Cons head tail) term) =  rewrite $ Cons head (Concat tail term)
rewrite (Snoc Nil a) = rewrite $ Cons a Nil
rewrite (Snoc (Cons head tail) a) = rewrite $ Cons head (Snoc tail a)
rewrite term = Just term
