module Sqnc where

import Control.Applicative hiding (many,some)

sqnc :: (Applicative m) => [m a] -> m [a]
sqnc []     = pure []
sqnc (c:cs) = (:) <$> c <*> sqnc cs

some :: Alternative f => f a -> f [a]
some v = (:) <$> v <*> many v

many :: Alternative f => f a -> f [a]
many v = some v <|> pure []
