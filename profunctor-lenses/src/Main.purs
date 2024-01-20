module Main where

import Prelude
import Data.List (List(..))
import Data.List as List
import Data.Lens.Lens (Lens, lens)
import Data.Lens.Fold ((^?))
import Data.Lens.Index (ix)
import Data.Lens.Getter (view)
import Data.Lens.Setter (set)
import Data.Maybe (fromMaybe)

import Effect (Effect)
import Effect.Console (log)

type Task = { name :: String, completed :: Boolean }

name :: Lens Task Task String String
-- name :: forall b. Lens { name :: String | b } { name :: String | b } String String
-- name :: forall a b. Lens { name :: a | b } { name :: a | b } a a
name = lens _.name _ { name = _ }

completed :: forall a b. Lens { completed :: a | b } { completed :: a | b } a a
completed = lens _.completed _ { completed = _ }

task :: Task
task = { name: "論文書け", completed: false }

main :: Effect Unit
main = do
  log $ view name task -- "論文書け"
  log $ show $ set completed true task -- { completed: true, name: "論文書け" }



説明: view name task はどんな値？
view :: forall s t a b. AGetter s t a b -> s -> a
view l = unwrap (l (Forget identity))

-- 型を具体化すると
view :: forall t a b. AGetter Task t a b -> Task -> a
view name task = unwrap (name (Forget identity)) task

さらに、
type AGetter s t a b = Fold a s t a b
type Fold r s t a b = Optic (Forget r) s t a b
type Optic p s t a b = p a b -> p s t
より
type AGetter Task t a b
= Fold a Task t a b
= Optic (Forget a) Task t a b
= (Forget a) a b -> (Forget a) Task t
よって
view :: forall t a b. ((Forget a) a b -> (Forget a) Task t) -> Task -> a
view name task = unwrap (name (Forget identity)) task

nameに注目
name :: Lens Task Task String String
ここで、
type Lens s t a b = forall p. Strong p => Optic p s t a b
type Optic p s t a b = p a b -> p s tより
より
name :: Lens Task Task String String
= forall p. Strong p => Optic p Task Task String String
= forall p. Strong p => p String String -> p Task Task
また、
newtype Forget r a b = Forget (a -> r)
より
Forget identity :: Forget a a b
また、nameの型より
Forget identity :: forall p. Strong p => p String String -> p Task Task
であることと、Forget r => Strong rより
Forget identity :: Forget String String String
よって、
name :: Forget String String String -> Forget String Task Task
name (Forget identity) :: Forget String Task Task
また
unwrap :: forall t a. Newtype t a => t -> a
type Forget r a b = Forget (a -> r)
より
unwrap (name (Forget identity)) :: Task -> String
よって
unwrap (name (Forget identity)) task :: String
文字列、つまりnameが取り出せていそう












-- view name task

-- name :: Lens Task Task String String
-- = forall p. Strong p => Optic p Task Task String String
-- = forall p. Strong p => p String String -> p Task Task

-- view :: forall s t a b. AGetter s t a b -> s -> a
-- view :: forall t b. AGetter Task t string b -> Task -> String

-- AGetter Task t String b
-- = Fold String Task t String b
-- = Optic (Forget String) Task t String b
-- = (Forget String) String b -> (Forget String) Task t

-- view l = unwrap (l (Forget identity))
-- view name task = (unwrap (name (Forget identity))) task
-- Forget identity :: Forget a a b

-- view name = unwrap (name (Forget identity))
-- name (Forget identity) :: Forget a Task Task
-- newtype Forget r a b = Forget (a -> r) と unwrap :: forall t a. Newtype t a => t -> a より
-- unwrap (name (Forget identity)) :: Task -> a
-- view name task :: a
-- a = String








-- { name :: a | b } ... a型のメンバnameをもつ任意のレコードb
-- type Optic p s t a b = p a b -> p s t
-- type Lens s t a b = forall p. Strong p => Optic p s t a b
-- title :: forall p. p a a -> p { title :: a | b } { title :: a | b }
-- newtype Forget r a b = Forget (a -> r)
-- Forget identity :: Forget a a b
-- l :: AGetter s t a b
-- = Fold a s t a b = Optic (Forget a) s t a b
-- = (Forget a) a b -> (Forget a) s t
-- name :: Lens { name :: a | b } { name :: a | b } a a
-- = forall p. Strong p => Optic p { name :: a | b } { name :: a | b } a a
-- = forall p. Strong p => p a a -> p { name :: a | b } { name :: a | b }
-- view l = unwrap (l (Forget identity))
-- view name task
-- = unwrap (name (Forget identity)) task
-- name :: (Forget a) a a -> Forget a { name :: a | b } { name :: a | b }
-- task :: Task
-- name :: (Forget a) a a -> Forget a Task Task
-- Forget identity :: Forget a a b
-- name (Forget identity) :: Forget a Task Task
-- view :: forall s t a b. AGetter s t a b -> s -> a
-- view name :: Task -> a
-- view name task :: a
-- a = string
-- name :: Lens Task Task String String


type TodoList = { title :: String, tasks :: List Task }

title :: forall a b. Lens { title :: a | b } { title :: a | b } a a
title = lens _.title _ { title = _ }

tasks :: forall a b. Lens { tasks :: a | b } { tasks :: a | b } a a
tasks = lens _.tasks _ { tasks = _ }

todoList :: TodoList
todoList =
  { title: "Today's todo"
  , tasks: List.fromFoldable
    [ { name: "meeting at 15:00", completed: false }
    , { name: "dog walk", completed: true }
    , { name: "dog walk", completed: false }
    ]
  }

