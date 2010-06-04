module Parser where

import Control.Monad
import Control.Applicative

----------------------------------------------------------------

data Parser a = Parser { parse :: String -> [(a,String)] }
--data Parser a = Parser { parse :: String -> Maybe (a,String) }

----------------------------------------------------------------

instance Functor Parser where
    f `fmap` p = return f <*> p

instance Applicative Parser where
    pure  = return
    (<*>) = ap
    
instance Alternative Parser where
    empty = mzero
    (<|>) = mplus

instance Monad Parser where
    return a = Parser $ \cs -> return (a,cs)
    p >>= f  = Parser $ \cs -> parse p cs >>= \(a,cs') -> parse (f a) cs'

instance MonadPlus Parser where
    mzero       = Parser $ \_  -> mzero
    p `mplus` q = Parser $ \cs -> parse p cs `mplus` parse q cs

----------------------------------------------------------------

item :: Parser Char
item = Parser f
  where
    f []     = mzero
    f (c:cs) = return (c,cs)

----------------------------------------------------------------

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = item >>= check
  where
    check c
      | f c       = return c
      | otherwise = mzero

char :: Char -> Parser Char
char c = satisfy (c ==)

----------------------------------------------------------------

string :: String -> Parser String
string []     = return []
string (c:cs) = (:) <$> char c <*> string cs
