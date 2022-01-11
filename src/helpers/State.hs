module State where

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap f st = State $ \s ->
    let (x, newS) = runState st s
     in (f x, newS)

instance Applicative (State s) where
  pure x = State $ \s -> (x, s)
  sf <*> sx = State $ \s ->
    let (f, newS) = runState sf s
        (x, newerS) = runState sx newS
     in (f x, newerS)

instance Monad (State s) where
  sx >>= sf = State $ \s ->
    let (x, newS) = runState sx s
     in runState (sf x) newS

evalState :: State s a -> s -> a
evalState st = fst . runState st

execState :: State s a -> s -> s
execState st = snd . runState st

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put a = State $ const ((), a)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)