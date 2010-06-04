module A where

import Control.Monad.State

data Exp = C Int | Exp :+ Exp | Exp :* Exp deriving Show

infixl 7 :*
infixl 6 :+

data Code = Push Int | Add | Mul deriving Show
type Program = [Code]

compile :: Exp -> Program
compile expr = reverse $ compile' expr []

compile' :: Exp -> Program -> Program
compile' (C x) xs = Push x : xs
compile' (r :+ l) xs = Add : compile' l (compile' r xs)
compile' (r :* l) xs = Mul : compile' l (compile' r xs)

----------------------------------------------------------------

{-
type Stack = [Int]

push :: Int -> Stack -> Stack
push x xs = x : xs

pop :: Stack -> (Int,Stack)
pop [] = error "pop"
pop (x:xs) = (x,xs)

exec :: Program -> Int
exec prog = exec' prog []

exec' :: Program -> Stack -> Int
exec' [] stack = fst (pop stack)
exec' (Push x:cs) stack0 = let stack1 = push x stack0
                           in exec' cs stack1
exec' (Add:cs) stack0 = let (x,stack1) = pop stack0
                            (y,stack2) = pop stack1
                            stack3 = push (x + y) stack2
                        in exec' cs stack3
exec' (Mul:cs) stack0 = let (x,stack1) = pop stack0
                            (y,stack2) = pop stack1
                            stack3 = push (x * y) stack2
                        in exec' cs stack3

-}

type Stack = [Int]

push :: Int -> State Stack ()
push x = do
    xs <- get
    put (x : xs)

pop :: State Stack Int
pop = do
    (x:xs) <- get
    put xs
    return x

exec :: Program -> Int
exec prog = evalState (exec' prog) []

exec' :: Program -> State Stack Int
exec' [] = pop
exec' (Push x:cs) = do
    push x
    exec' cs
exec' (Add:cs) = do
    x <- pop
    y <- pop
    push (x + y)
    exec' cs
exec' (Mul:cs) = do
    x <- pop
    y <- pop
    push (x * y)
    exec' cs
