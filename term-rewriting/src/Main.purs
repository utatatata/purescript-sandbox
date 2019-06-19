module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log, logShow)
import Data.Show (show)
import Trs

main :: Effect Unit
main =
  let
    term1 = (Concat (Cons 0 (Cons 1 (Cons 2 Nil))) (Cons 3 (Cons 4 Nil)))
    term2 = (Snoc (Cons 0 (Cons 1 (Cons 2 Nil))) 3)
  in do
  logShow term1
  log $ "rewrite: " <> (show $ rewrite term1)
  logShow term2
  log $ "rewrite: " <> (show $ rewrite term2)
