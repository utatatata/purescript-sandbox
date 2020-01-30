module Data.Monad.Free where

import Prelude

data Free f a
  = Pure a
  | Free (f (Free f a))

instance functorFree :: Functor f => Functor (Free f) where
  map f (Pure x) = Pure (f x)
  map f (Free x) = Free (flip map x (map f))

instance applyFree :: Functor f => Apply (Free f) where
  apply (Pure f) (Pure x) = Pure (f x)
  apply (Free f) (Pure x) = Free (map (flip apply (Pure x)) f)
  apply f (Free x) = Free (map (apply f) x)

instance applicativeFree :: Functor f => Applicative (Free f) where
  pure = Pure

instance bindFree :: Functor f => Bind (Free f) where
  bind (Free x) f = Free (map (flip bind f) x)
  bind (Pure x) f = f x

instance monadFree :: Functor f => Monad (Free f)
