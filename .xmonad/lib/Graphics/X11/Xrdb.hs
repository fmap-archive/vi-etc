{-# LANGUAGE NamedFieldPuns #-}

module Graphics.X11.Xrdb (
  Resource(..),
  getResources,
  getResource,
  setResource
) where

-- A lightweight resource manager, wrapping `xrdb`, until I get
-- https://github.com/NixOS/nixpkgs/pull/2869 fixed. :-)

import Control.Applicative ((<$>))
import Control.Monad (void)
import Data.List (find, findIndex)
import Data.Maybe (fromMaybe)
import System.Process (readProcess)

data Resource = Resource
  { key   :: String
  , value :: String
  } deriving (Show)

getResources :: IO [Resource]
getResources = readProcess "xrdb" ["-query"] "" >>= \res -> return . (<$> lines res) $ \line -> do
  let position = fromMaybe 0 $ findIndex (==':') line
  uncurry Resource $ dropWhile (`elem` "\t:") <$> splitAt position line

getResource :: String -> IO (Maybe Resource)
getResource query = find ((== query) . key) <$> getResources

setResource :: Resource -> IO ()
setResource Resource{key,value} = void $ readProcess "xrdb" ["-override"] input
  where input = concat [key, ":", value]
