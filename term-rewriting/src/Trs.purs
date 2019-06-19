module Trs where

import Prelude (class Show, class Eq, show, ($), (<$>), (<>), (<@>), (>>=), identity, pure, flip, bind)
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
rewriteCPS term = go term identity
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
rewriteCont Nil = Just Nil

rewriteCont (Cons head tail) =
  (flip runCont) identity
    $ do
        fixed <- pure $ rewriteCont tail
        pure $ (Cons head) <$> fixed

rewriteCont (Concat Nil term) = rewrite term

rewriteCont (Concat (Cons head tail) term) = rewrite $ Cons head (Concat tail term)

rewriteCont (Concat term1 term2) =
  (flip runCont) identity
    $ do
        fixed <- pure $ rewriteCont term1
        pure $ Concat <$> fixed <@> term2 >>= rewriteCont

rewriteCont (Snoc Nil last) = rewrite $ Cons last Nil

rewriteCont (Snoc (Cons head tail) last) = rewrite $ Cons head (Snoc tail last)

rewriteCont (Snoc term last) =
  (flip runCont) identity
    $ do
        fixed <- pure $ rewriteCont term
        pure $ Snoc <$> fixed <@> last >>= rewriteCont
