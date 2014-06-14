{-# LANGUAGE RecordWildCards, ViewPatterns, LambdaCase #-}

module Graphics.X11.Monitor (
  Monitor(..),
  getMonitors,
  isRetina,
  configureDisplays
) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((&&&))
import Control.Monad (liftM2, filterM, void, forM_)
import Data.Function (on)
import Data.Functor.Extras ((<$$>),(<$$$>))
import Data.List (nub, find)
import Data.Maybe (catMaybes)
import Data.Maybe (fromJust)
import Data.Units (Convertible(convert), Inches(..), Millimeters(..))
import Graphics.X11.Types (Rotation, xRR_Rotate_270)
import Graphics.X11.Xlib (Display, Window, openDisplay, defaultScreen, rootWindow, createSimpleWindow, blackPixel)
import Graphics.X11.Xlib.Display (defaultRootWindow)
import Graphics.X11.Xlib.Extras (currentTime)
import Graphics.X11.Xrandr (XRROutputInfo(..), XRRCrtcInfo(..), XRRModeInfo(..), XRRScreenResources(..), xrrGetOutputInfo, xrrGetScreenResources, xrrGetCrtcInfo, xrrGetScreenInfo, xrrSetScreenConfig, xrrConfigCurrentConfiguration)
import System.Process (readProcess)

data Monitor = Monitor
  { dimensions :: (Inches, Inches)
  , resolution :: (Double, Double)
  } deriving (Show, Eq)

instance Ord Monitor where
  Monitor _ (a, b) <= Monitor _ (c, d) = a * b <= c * d

withPreferredUnits :: Convertible a b => (c -> a) -> (c -> a) -> c -> (b, b)
withPreferredUnits = (&&&) `on` fmap convert

ppi :: Monitor -> Double
ppi Monitor{..} = dp / di
  where di = sqrt $ on (+) ((**2) . getInches) `uncurry` dimensions
        dp = sqrt $ on (+) (**2) `uncurry` resolution

isRetina :: Monitor -> Bool
isRetina = (>= 220) . ppi

getMonitors :: IO [Monitor]
getMonitors = openDisplay [] >>= \display -> getScreenResources display >>= \case
  Just resources -> do
    output <- filter isActive . catMaybes <$> xrrGetOutputInfo display resources `mapM` xrr_sr_outputs resources
    let dimensions  = getDimensions `map` output
        resolutions = map maximum <$> getResolutions display resources `mapM` output
    zipWith Monitor dimensions <$> resolutions
  Nothing -> return []

configureDisplays :: IO () -- TODO: abstract and move vi-specific logic to xmonad.hs
configureDisplays = openDisplay [] >>= \display -> getScreenResources display >>= \case
  Just resources -> do
    connected <- filter isConnected . catMaybes <$> xrrGetOutputInfo display resources `mapM` xrr_sr_outputs resources
    if length connected == 1 then return () {- If there's only one connected display, do nothing. -} else do 
      forM_ connected $ \output-> do
        -- Disable the primary display, I always clamshell:
        if xrr_oi_name output == "eDP1" then disable output else do
          -- Enable all other displays!
          enable output
          -- Left-rotate any 1920x1200 displays, assuming they're the U2412s I use at work.
          maximum <$> getResolutions display resources output >>= \case
            (1920,1200) -> rotateLeft output
            otherwise   -> return ()
    pairWise rightOf $ filter isActive connected
  Nothing -> return () -- ???

pairWise :: Monad m => (a -> a -> m b) -> [a] -> m ()
pairWise _ []       = return ()
pairWise _ (a:[])   = return ()
pairWise f (a:b:xs) = f a b >> pairWise f (b:xs)

-- Some day I will rewrite these functions to use the <X11/extensions/Xrandr.h>
-- more directly. Today is not that day. 
-- <<EOAAAAAAAAGH

rotateLeft :: XRROutputInfo -> IO ()
rotateLeft (xrr_oi_name -> screenName) = withXrandr
  [ "--output"
  , screenName
  , "--rotation"
  , "left"
  ]

rightOf :: XRROutputInfo -> XRROutputInfo -> IO ()
rightOf (xrr_oi_name -> left) (xrr_oi_name -> right) = withXrandr
  [ "--output"
  , right
  , "--right-of"
  , left
  ]

disable :: XRROutputInfo -> IO ()
disable (xrr_oi_name -> screenName) = withXrandr
  [ "--output"
  , screenName
  , "--off"
  ]

enable :: XRROutputInfo -> IO ()
enable (xrr_oi_name -> screenName) = withXrandr
  [ "--output"
  , screenName
  , "--auto"
  ]

withXrandr :: [String] -> IO ()
withXrandr = void . flip (readProcess "xrandr") []

--EOAAAAAAAAGH

isConnected :: XRROutputInfo -> Bool
isConnected = (==0) . xrr_oi_connection

isActive :: XRROutputInfo -> Bool
isActive = (/=0) . xrr_oi_crtc 

getDimensions :: XRROutputInfo -> (Inches, Inches)
getDimensions = withPreferredUnits xrr_oi_mm_width xrr_oi_mm_height

getResolutions :: Display -> XRRScreenResources -> XRROutputInfo -> IO [(Double, Double)]
getResolutions display resources output = fmap (filter offAxis . catMaybes)
                                        $ withPreferredUnits xrr_ci_width xrr_ci_height 
                                    <$$$> xrrGetCrtcInfo display resources 
                                    `mapM` xrr_oi_crtcs output

offAxis :: Num a => Ord a => (a, a) -> Bool
offAxis = flip (>) 0 . fst

getScreenResources :: Display -> IO (Maybe XRRScreenResources)
getScreenResources = liftM2 (>>=) emptyWindow xrrGetScreenResources

emptyWindow :: Display -> IO Window
emptyWindow display = do
  let screen = defaultScreen display
      pixel  = blackPixel display screen
  root <- rootWindow display screen
  createSimpleWindow display root 0 0 1 1 0 pixel pixel
