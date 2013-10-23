module Util.Display where

import Graphics.X11.Xlib (openDisplay)
import Graphics.X11.Xlib.Types
import Graphics.X11.Xinerama (getScreenInfo)
import System.IO.Unsafe

getDisplayDimensions :: Int -> (Int,Int)
getDisplayDimensions index = (getRectangleX display, getRectangleY display)
 where display                           = (unsafePerformIO $ openDisplay [] >>= getScreenInfo) !! index
       getRectangleX (Rectangle _ _ x _) = (fromIntegral x)
       getRectangleY (Rectangle _ _ _ y) = (fromIntegral y) 
