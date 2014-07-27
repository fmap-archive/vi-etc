{-# LANGUAGE QuasiQuotes, RecordWildCards #-}

-- Based on Gwern's, natch. I want to supply non-URL arguments to browsers..

module XMonad.Actions.Search.Extras (
  Browser(..),
  promptSearchBrowser',
  selectSearchBrowser',
  ddg
) where

import Data.Functor.Extras ((<$$>))
import Data.List (intercalate, isPrefixOf)
import Data.String.Interpolate (i)
import XMonad.Actions.Search (Site, Query)
import XMonad.Core (X(..), spawn)
import XMonad.Prompt (XPrompt(..), XPConfig(..), historyCompletionP, mkXPrompt, getNextCompletion)
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.XSelection (getSelection)

data Browser = Browser
  { executable :: FilePath
  , options :: [String]
  }

instance Show Browser where
  show Browser{..} = [i|#{executable} #{intercalate " " options}|]

ddg :: Site
ddg = ("https://duckduckgo.com?q="++)

search :: Browser -> Site -> Query -> X ()
search Browser{..} site query = safeSpawn executable $ options ++ [site query]

selectSearchBrowser' :: Browser -> Site -> X ()
selectSearchBrowser' = (=<< getSelection) <$$> search

data Search = Search

instance XPrompt Search where
  showXPrompt       = const "Search: "
  nextCompletion    = const getNextCompletion
  commandToComplete = const id

promptSearchBrowser' :: XPConfig -> Browser -> Site -> X ()
promptSearchBrowser' config = mkXPrompt Search config completion <$$> search
  where completion = historyCompletionP $ isPrefixOf "Search"
