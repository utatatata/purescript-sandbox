module Control.Monad.Reader where

import Control.Apply (class Apply)
import Control.Applicative (class Applicative)
import Control.Bind (class Bind)
import Control.Monad (class Monad)
import Data.Functor (class Functor)

newtype Reader r a
  = Reader (r -> a)

runReader :: forall a r. Reader r a -> r -> a
runReader (Reader ra) r =
  let
    a = ra r
  in
    a

withReader :: forall a r1 r2. (r2 -> r1) -> Reader r1 a -> Reader r2 a
withReader f (Reader r1a) =
  Reader \r2 ->
    let
      r1 = f r2

      a = r1a r1
    in
      a

instance functorReader :: Functor (Reader r) where
  map :: forall a b. (a -> b) -> Reader r a -> Reader r b
  map f (Reader ra) =
    Reader \r ->
      let
        a = ra r
      in
        f a

instance applyReader :: Apply (Reader r) where
  apply :: forall a b. Reader r (a -> b) -> Reader r a -> Reader r b
  apply (Reader rf) (Reader ra) =
    Reader \r ->
      let
        f = rf r

        a = ra r

        b = f a
      in
        b

instance applicativeReader :: Applicative (Reader r) where
  pure :: forall a. a -> Reader r a
  pure a = Reader \r -> a

instance bindReader :: Bind (Reader r) where
  bind :: forall a b. Reader r a -> (a -> Reader r b) -> Reader r b
  bind (Reader ra) f =
    Reader \r ->
      let
        a = ra r

        rrb = f a
      in
        case rrb of
          Reader rb ->
            let
              b = rb r
            in
              b

instance monadReader :: Monad (Reader r)

ask :: forall r. Reader r r
ask = Reader \r -> r

local :: forall a r. (r -> r) -> Reader r a -> Reader r a
local = withReader
