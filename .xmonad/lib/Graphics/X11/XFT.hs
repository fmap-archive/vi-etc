module Graphics.X11.XFT (XFTSpecification(..)) where

import Data.List (intercalate)

data XFTSpecification = XFTSpecification
  { family :: String
  , style  :: Maybe String
  , size   :: Maybe Int
  }

instance Show XFTSpecification where
  show (XFTSpecification family style size) = 
    intercalate ":".filter (not.null) $ 
      ["xft", family, showStyle style, showSize size]
    

showStyle :: Maybe String -> String
showStyle Nothing = []
showStyle (Just st) = "style="++st

showSize :: Maybe Int -> String
showSize Nothing = []
showSize (Just sz) = "size="++(show sz)
