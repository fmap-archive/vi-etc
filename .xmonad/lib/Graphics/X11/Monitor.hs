{-# LANGUAGE RecordWildCards, LambdaCase #-}

module Graphics.X11.Monitor (
  Monitor(..),
  getMonitors,
  isRetina
) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((&&&))
import Control.Monad (liftM2)
import Data.Function (on)
import Data.Functor.Extras ((<$$$>))
import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.Units (Convertible(convert), Inches(..), Millimeters(..))
import Graphics.X11.Xlib (Display, Window, openDisplay, defaultScreen, rootWindow, createSimpleWindow, blackPixel)
import Graphics.X11.Xrandr (XRROutputInfo(..), XRRCrtcInfo(..), XRRModeInfo(..), XRRScreenResources(..), xrrGetOutputInfo, xrrGetScreenResources, xrrGetCrtcInfo)

data Monitor = Monitor
  { dimensions :: (Inches, Inches)
  , resolution :: (Double, Double)
  } deriving (Show, Eq)

instance Ord Monitor where
  Monitor _ (a, b) <= Monitor _ (c, d) = a * b <= c * d

withPreferredUnits :: Convertible a b => (c -> a) -> (c -> a) -> c -> (b, b)
withPreferredUnits = (&&&) `on` fmap convert

ppi :: Monitor -> Double
ppi Monitor{..} = pixels / inches
  where inches = on (*) getInches `uncurry` dimensions
        pixels = uncurry (*) resolution

isRetina :: Monitor -> Bool
isRetina = (>= 220) . ppi

getMonitors :: IO [Monitor]
getMonitors = openDisplay [] >>= \display -> getScreenResources display >>= \case
  Just resources -> do
    output <- catMaybes <$> xrrGetOutputInfo display resources `mapM` xrr_sr_outputs resources
    let dimensions  = getDimensions `map` output
        resolutions = nub . concat <$> getResolutions display resources `mapM` output
    zipWith Monitor dimensions <$> resolutions
  Nothing -> return []

getDimensions :: XRROutputInfo -> (Inches, Inches)
getDimensions = withPreferredUnits xrr_oi_mm_width xrr_oi_mm_height

getResolutions :: Display -> XRRScreenResources -> XRROutputInfo -> IO [(Double, Double)]
getResolutions display resources output = fmap (filter hasResolution . catMaybes)
                                        $ withPreferredUnits xrr_ci_width xrr_ci_height 
                                    <$$$> xrrGetCrtcInfo display resources 
                                    `mapM` xrr_oi_crtcs output

hasResolution :: Num a => Ord a => (a, a) -> Bool
hasResolution = flip (>) 0 . fst

getScreenResources :: Display -> IO (Maybe XRRScreenResources)
getScreenResources = liftM2 (>>=) emptyWindow xrrGetScreenResources

emptyWindow :: Display -> IO Window
emptyWindow display = do
  let screen = defaultScreen display
      pixel  = blackPixel display screen
  root <- rootWindow display screen
  createSimpleWindow display root 0 0 1 1 0 pixel pixel
