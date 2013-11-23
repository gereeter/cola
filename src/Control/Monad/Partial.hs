module Control.Monad.Partial where

import Control.Applicative
import Control.Monad

data Partial a = Step (Partial a) | Stop a

instance Functor Partial where
    fmap f (Step m) = Step (fmap f m)
    fmap f (Stop a) = Stop (f a)

instance Applicative Partial where
    pure = return
    (<*>) = ap

instance Monad Partial where
    return = Stop
    Step m >>= f = Step (m >>= f)
    Stop a >>= f = f a

run :: Int -> Partial a -> Partial a
run n m | n <= 0 = m
run n m@(Stop _) = m
run n (Step m) = run (n - 1) m

finish :: Partial a -> a
finish (Step m) = finish m
finish (Stop a) = a

step :: Partial ()
step = Step (Stop ())

never :: Partial a
never = Step never
