module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log, logShow)
import Trs

main :: Effect Unit
main =
  let
    term1 = (Concat (Cons 0 (Cons 1 (Cons 2 Nil))) (Cons 3 (Cons 4 Nil)))
  in do
  logShow term1
  logShow $ rewrite term1
