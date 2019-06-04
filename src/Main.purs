module Main where

import Prelude
import Data.List
import Data.Traversable
import Data.Unfoldable
import Effect (Effect)
import Effect.Console (log)
import Hyper

applyN :: forall a. Int -> (a -> a) -> (a -> a)
applyN n f =
  foldr (<<<) identity
    ((replicate n f) :: List (a -> a))

main :: Effect Unit
main =
  let
    exp1 = Hyper (Nat 2) 2 (Nat 3)
    exp2 = Hyper (Hyper (Nat 1) 2 (Nat 1)) 1 (Nat 2)
  in
    do
      log $ show $ exp1
      log $ "-> " <> (show $ transform exp1)
      log $ show exp2
      log $ "-> " <> (show $ exp2 # transform1)
      log $ "-> " <> (show $ exp2 # transform1 <<< transform1)
      log $ "-> " <> (show $ exp2 # applyN 2 transform1)
      log $ "-> " <> (show $ exp2 # applyN 3 transform1)
      log $ "-> " <> (show $ exp2 # applyN 4 transform1)
      log $ "-> " <> (show $ exp2 # applyN 5 transform1)
      log $ "-> " <> (show $ exp2 # applyN 6 transform1)
      log $ "-> " <> (show $ exp2 # applyN 7 transform1)
      log $ "-> " <> (show $ exp2 # applyN 8 transform1)
