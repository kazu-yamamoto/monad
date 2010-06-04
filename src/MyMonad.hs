module Foo where

class Functor_ m where
    (<$>) :: (a -> b) -> m a -> m b

class Functor_ m => Pointed_ m where
    pure_ :: a -> m a

class Pointed_ m => Applicative_ m where
    (<*>) :: m (a -> b) -> m a -> m b

class Applicative_ m => Monad_ m where
    join :: m (m a) -> m a

(>==) :: (Monad_ m) => m a -> (a -> m b) -> m b
m >== f = join $ f <$> m

instance Functor_ [] where
    _ <$> [] = []
    f <$> (x:xs) = f x : f <$> xs -- aka map

instance Pointed_ [] where
    pure_ x = [x]

instance Applicative_ [] where
    [] <*> _ = []
    (f:fs) <*> xs = f <$> xs ++ fs <*> xs
--    fs <*> xs  = foldr (\f rs -> f <$> xs ++ rs) [] fs
    
instance Monad_ [] where
    join = concat -- foldr (++) []

-- (+) <$> [0,1] <*> [2,3]


instance Functor_ ((->) e) where
    (<$>) = (.) -- b combinator

instance Pointed_ ((->) e) where
    pure_ = const -- k combinator

instance Applicative_ ((->) e) where
    (<*>) f g x = f x (g x) -- s combinator
    
instance Monad_ ((->) e) where
    join f x = f x x -- w combinator

-- ((+) <$> id <*> \x -> x + 1) 2

