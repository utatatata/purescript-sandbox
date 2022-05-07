module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)
import Control.Monad.Free (wrap)
import Control.Monad.Trampoline (Trampoline, delay, done, runTrampoline)

-- x_n = n * x_{x-1}
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

-- x_0 = 0
-- x_1 = 1
-- x_n = x_{n-2} + x_{n-1} (n > 2)
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

even :: Int -> Boolean
even n = if n <= 0 then true else odd (n - 1)

odd :: Int -> Boolean
odd n = if n <= 0 then false else even (n - 1)

fact_acc :: Int -> Int -> Int
fact_acc acc 0 = acc
fact_acc acc n = fact_acc (n * acc) (n - 1)

fib_acc :: Int -> Int -> Int -> Int
fib_acc a b 0 = a
fib_acc a b n = fib_acc b (a + b) (n - 1)

fact_trampoline :: Int -> Trampoline Int
fact_trampoline 0 = done 1
fact_trampoline n =
  wrap $ \_ -> do
    m <- fact_trampoline (n - 1)
    pure $ n * m

fib_trampoline :: Int -> Trampoline Int
fib_trampoline 0 = done 0
fib_trampoline 1 = done 1
fib_trampoline n = wrap \_ -> do
  l <- fib_trampoline (n - 2)
  m <- fib_trampoline (n - 1)
  pure $ m + l

even_trampoline :: Int -> Trampoline Boolean
even_trampoline n =
  if n <= 0 then
    done true
  else
    wrap \_ -> odd_trampoline (n - 1)

odd_trampoline :: Int -> Trampoline Boolean
odd_trampoline n =
  if n <= 0 then
    done false
  else
    wrap \_ -> even_trampoline (n - 1)

main :: Effect Unit
main = do
  logShow $ fact 5
  logShow $ fact_acc 5 1
  logShow $ runTrampoline $ fact_trampoline 5
  -- logShow $ fact 20000
  -- logShow $ fact_acc 20000 1
  logShow $ runTrampoline $ fact_trampoline 20000
  -- logShow $ fib_acc 10 0 1
  -- logShow $ fib_cps 10 identity
  -- logShow $ runTrampoline $ even_trampoline 20000
  -- logShow $ runTrampoline $ fib_trampoline 10
