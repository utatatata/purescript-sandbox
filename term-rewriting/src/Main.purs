module Main where

import Prelude (Unit, discard, show, ($), (<>))
import Effect (Effect)
import Effect.Console (log, logShow)
import Trs (Term(..), rewrite, rewriteCPS, rewriteCont)

main :: Effect Unit
main =
  let
    term1 = (Concat (Cons 0 (Cons 1 (Cons 2 Nil))) (Cons 3 (Cons 4 Nil)))

    term2 = (Snoc (Cons 0 (Cons 1 (Cons 2 Nil))) 3)

    term3 = (Concat (Concat (Cons 0 Nil) (Cons 1 Nil)) (Cons 2 Nil))

    term4 =
      ( Concat
        ( Concat
          ( Concat
            ( Concat (Cons 0 Nil) (Cons 1 Nil)
            ) (Cons 2 Nil)
          ) (Cons 3 Nil)
        ) (Cons 4 Nil)
      )
  in
    do
      logShow term1
      log $ "rewrite: " <> (show $ rewrite term1)
      logShow term2
      log $ "rewrite: " <> (show $ rewrite term2)
      logShow term3
      log $ "rewrite: " <> (show $ rewrite term3)
      logShow term4
      log $ "rewrite: " <> (show $ rewrite term4)
      logShow term1
      log $ "rewriteCPS: " <> (show $ rewriteCPS term1)
      logShow term2
      log $ "rewriteCPS: " <> (show $ rewriteCPS term2)
      logShow term3
      log $ "rewriteCPS: " <> (show $ rewriteCPS term3)
      logShow term4
      log $ "rewriteCPS: " <> (show $ rewriteCPS term4)
      logShow term1
      log $ "rewriteCont: " <> (show $ rewriteCont term1)
      logShow term2
      log $ "rewriteCont: " <> (show $ rewriteCont term2)
      logShow term3
      log $ "rewriteCont: " <> (show $ rewriteCont term3)
      logShow term4
      log $ "rewriteCont: " <> (show $ rewriteCont term4)
