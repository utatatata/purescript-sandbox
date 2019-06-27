module Trs where

import Prelude (class Eq, class Show, bind, identity, pure, show, ($), (<$>), (<>), (<@>), (>>=))
import Control.Monad.Cont (runCont)
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

rewrite (Concat (Cons head tail) term) = rewrite $ Cons head (Concat tail term)

rewrite (Concat term1 term2) = Concat <$> (rewrite term1) <@> term2 >>= rewrite

rewrite (Snoc Nil last) = rewrite $ Cons last Nil

rewrite (Snoc (Cons head tail) last) = rewrite $ Cons head (Snoc tail last)

rewrite (Snoc term last) = Snoc <$> (rewrite term) <@> last >>= rewrite

rewriteCPS :: forall a. Term a -> Maybe (Term a)
rewriteCPS t = go t identity
  where
  go :: Term a -> (Maybe (Term a) -> Maybe (Term a)) -> Maybe (Term a)
  go Nil cont = cont $ Just Nil

  go (Cons head tail) cont = go tail (\fixed -> cont $ (Cons head) <$> fixed)

  go (Concat Nil term) cont = go term cont

  go (Concat (Cons head tail) term) cont = go (Cons head (Concat tail term)) cont

  go (Concat term1 term2) cont = go term1 (\fixed -> Concat <$> fixed <@> term2 >>= go <@> cont)

  go (Snoc Nil last) cont = go (Cons last Nil) cont

  go (Snoc (Cons head tail) last) cont = go (Cons head (Snoc tail last)) cont

  go (Snoc term last) cont = go term (\fixed -> Snoc <$> fixed <@> last >>= go <@> cont)

rewriteCont :: forall a. Term a -> Maybe (Term a)
rewriteCont t = runCont (match t) identity
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
