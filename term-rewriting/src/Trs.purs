module Trs where

import Prelude (class Show, class Eq, show, ($), (<@>), (<$>), (<>))
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
rewrite Nil = Just Nil
rewrite (Cons head tail) = (Cons head) <$> (rewrite tail)

rewrite (Concat Nil term) = rewrite term
rewrite (Concat (Cons head tail) term) =  rewrite $ Cons head (Concat tail term)
rewrite (Concat term1 term2) = Concat <$> (rewrite term1) <@> term2

rewrite (Snoc Nil last) = rewrite $ Cons last Nil
rewrite (Snoc (Cons head tail) last) = rewrite $ Cons head (Snoc tail last)
rewrite (Snoc term last) = Snoc <$> (rewrite term) <@> last


