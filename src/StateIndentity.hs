module State where

import Control.Monad.Identity
import Control.Monad.State

cons :: Char -> String -> Identity String
cons x = \xs -> return (x:xs)

type Counter = Int
type SIdentity a = StateT Counter Identity a

runSIdentity :: SIdentity a -> Counter -> (a, Counter)
runSIdentity m initState = runIdentity (runStateT m initState)

inc :: SIdentity ()
inc = get >>= \x -> put (x+1)

consS :: Char -> String -> SIdentity String
consS x = \xs -> do
    inc
    return (x:xs)
