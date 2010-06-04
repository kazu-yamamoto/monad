module ApplicativeList where

(<*>) :: [a -> b] -> [a] -> [b]
[]     <*> _ = []
(f:fs) <*> xs = f `map` xs ++ (fs <*> xs)
