module Trs where

import Prelude (class Eq, class Show, bind, identity, pure, show, ($), (<$>), (<>), (<@>))
import Control.Monad.Cont (runCont)
import Data.Maybe (Maybe(..))

data List a
  = Nil
  | Cons a (List a)
  | Concat (List a) (List a)
  | Snoc (List a) a

derive instance eqListInt :: Eq (List Int)

instance showListInt :: Show (List Int) where
  show Nil = "Nil"
  show (Cons x y) = "(Cons " <> show x <> " " <> show y <> ")"
  show (Concat x y) = "(Concat " <> show x <> " " <> show y <> ")"
  show (Snoc x y) = "(Snoc " <> show x <> " " <> show y <> ")"

rewrite :: forall a. List a -> Maybe (List a)
rewrite t = runCont (match t) identity
  where
  go (Just term) = match term

  go Nothing = pure Nothing

  match Nil = pure $ Just Nil

  match (Cons head tail) = do
    fixed <- match tail
    pure $ (Cons head) <$> fixed

  match (Concat Nil term) = match term

  match (Concat (Cons head tail) term) = match $ Cons head (Concat tail term)

  match (Concat term1 term2) = do
    fixed <- match term1
    go $ Concat <$> fixed <@> term2

  match (Snoc Nil last) = match $ Cons last Nil

  match (Snoc (Cons head tail) last) = match $ Cons head (Snoc tail last)

  match (Snoc term last) = do
    fixed <- match term
    go $ Snoc <$> fixed <@> last
