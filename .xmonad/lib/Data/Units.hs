{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Data.Units (
  Convertible(..),
  Inches(..), 
  Millimeters(..)
) where

import Foreign.C.Types (CULong(..), CUInt(..))

newtype Millimeters = Millimeters { getMillimeters :: Double } deriving (Show, Eq, Ord)
newtype Inches      = Inches      { getInches      :: Double } deriving (Show, Eq, Ord)

class Convertible a b where
  convert :: a -> b

instance Convertible Millimeters Inches where
  convert = Inches . (/25.4) . getMillimeters

instance Convertible CULong Inches where
  convert = convert . Millimeters . convert

instance Convertible CULong Double where
  convert = fromIntegral

instance Convertible CUInt Double where
  convert = fromIntegral
