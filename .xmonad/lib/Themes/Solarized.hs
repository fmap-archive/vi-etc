module Themes.Solarized where

import XMonad.Prompt
import XMonad.Layout.Decoration
import Data.List
import Util.Dzen2

base3   = "#002b36"
base2   = "#073642"
base1   = "#586e75"
base0   = "#657b83"
base00  = "#839496"
base01  = "#93a1a1"
base02  = "#eee8d5"
base03  = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"

solarizedTheme :: Theme
solarizedTheme = defaultTheme 
  { activeColor         = base03 
  , inactiveColor       = base03 
  , urgentColor         = base03 
  , activeBorderColor   = base01 
  , inactiveBorderColor = base02
  , urgentBorderColor   = base02
  , activeTextColor     = base00
  , inactiveTextColor   = base01
  , urgentTextColor     = base01
  }

solarizedXPConfig :: XPConfig
solarizedXPConfig = defaultXPConfig
  { bgColor       = base03
  , fgColor       = base01
  }

solarizedDzenConfig :: Dzen2Config
solarizedDzenConfig = dzen
  { bg = Just base03
  , fg = Just base00
  }
