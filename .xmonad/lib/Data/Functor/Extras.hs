module Data.Functor.Extras (
  (??),
  (<$$>),
  (<$$$>)
) where

(??) :: Functor f => f a -> (a -> b) -> f b
(??) = flip fmap

(<$$>) :: Functor f => Functor g => (a -> b) -> g (f a) -> g (f b)
(<$$>) = fmap fmap fmap

(<$$$>) :: Functor f => Functor g => Functor h => (a -> b) -> h (g (f a)) -> h (g (f b))
(<$$$>) = fmap (<$$>) fmap
infixr 2 <$$$>
