module State where

import Prelude
import Data.Tuple (Tuple(..), fst, snd)

type Internal s a
  = { result :: a, state :: s }

newtype State s a
  = State (s -> Tuple a s)

runState :: forall s a. State s a -> s -> Tuple a s
runState (State m) = m

evalState :: forall s a. State s a -> s -> a
evalState (State m) s = fst $ m s

execState :: forall s a. State s a -> s -> s
execState (State m) s = snd $ m s

mapState :: forall s a b. (Tuple a s -> Tuple b s) -> State s a -> State s b
mapState f (State m) = State (f <<< m)

withState :: forall s a. (s -> s) -> State s a -> State s a
withState f (State m) = State (m <<< f)

pure :: forall s a. a -> State s a
pure a = State \s -> Tuple a s

bind :: forall s a b. State s a -> (a -> State s b) -> State s b
bind (State m) f =
  State \s ->
    m s # \(Tuple a s') -> case f a of State st -> st s'

state :: forall s a. (s -> Tuple a s) -> State s a
state f = State f

get :: forall s. State s s
get = state \s -> Tuple s s

gets :: forall s a. (s -> a) -> State s a
gets f = state \s -> Tuple (f s) s

put :: forall s. s -> State s Unit
put s = state \_ -> Tuple unit s

modify :: forall s. (s -> s) -> State s s
modify f = state \s -> let s' = f s in Tuple s' s'

modify_ :: forall s. (s -> s) -> State s Unit
modify_ f = state \s -> Tuple unit $ f s
