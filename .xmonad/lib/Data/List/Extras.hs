module Data.List.Extras ((-:)) where

(-:) :: [a] -> a -> [a]
(-:) xs x = xs ++ [x]
infixr 5 -:
