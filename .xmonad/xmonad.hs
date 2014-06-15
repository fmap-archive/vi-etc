{-# LANGUAGE RecordWildCards, FlexibleContexts, Rank2Types, ViewPatterns, QuasiQuotes #-}

import Control.Monad (liftM2)
import Data.Functor.Extras ((??))
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, find, nub)
import Data.List.Extras ((-:))
import Data.Map (Map, empty)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(mconcat), All)
import Data.String.Interpolate (i)
import GHC.Word (Word64)
import Graphics.X11.Dzen2 ()
import Graphics.X11.Monitor (Monitor(..), getMonitors, isRetina, configureDisplays)
import Graphics.X11.Types (Window, KeyMask, KeySym, mod4Mask)
import Graphics.X11.XFT (XFTSpecification(..))
import Graphics.X11.Xlib.Extras (Event)
import Graphics.X11.Xrdb (Resource(..), setResource)
import XMonad.Actions.CopyWindow (copy, kill1)
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.Search (promptSearchBrowser, selectSearchBrowser, searchEngine, SearchEngine)
import XMonad.Core (X(..), XConfig(..), LayoutClass, ManageHook, spawn, whenJust)
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen, isDialog, doCenterFloat)
import XMonad.Hooks.Minimize (minimizeEventHook)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout (Full(..))
import XMonad.Layout.Decoration (Decoration, DefaultShrinker)
import XMonad.Layout.Fullscreen (fullscreenEventHook, fullscreenManageHook)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.Minimize (Minimize, minimize, minimizeWindow, MinimizeMsg(RestoreNextMinimizedWin))
import XMonad.Layout.Simplest (Simplest)
import XMonad.Layout.Tabbed (tabbed, TabbedDecoration, Theme(..), shrinkText)
import XMonad.Main (xmonad)
import XMonad.ManageHook ((-->), (=?), className, title, doShift)
import XMonad.ManageHook.Query (icon, name)
import XMonad.Operations (withFocused, windows, sendMessage, screenWorkspace)
import XMonad.Prompt (XPConfig(..), XPPosition(..))
import XMonad.Prompt.Keymap.CtrlP (ctrlP)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.StackSet (sink, shift, view, greedyView, focusUp, focusDown)
import XMonad.Themes.Solarized (base02, base01, solarizedTheme, solarizedXPConfig)
import XMonad.Util.EZConfig (mkKeymap)
      
configuration :: Monitor -> XConfig Layout
configuration monitor = XConfig
  { terminal           = terminal'
  , workspaces         = workspaces'
  , borderWidth        = 2
  , normalBorderColor  = base02
  , focusedBorderColor = base01
  , manageHook         = mconcat manageHooks
  , handleEventHook    = mconcat eventHooks
  , startupHook        = setWMName "LG3D"
  , logHook            = return ()
  , focusFollowsMouse  = False
  , layoutHook         = layoutHook' monitor
  , modMask            = mod4Mask
  , keys               = keys' monitor
  , mouseBindings      = const empty
  , clickJustFocuses   = False
  }

terminal', browser' :: String
terminal' = "urxvt"
browser'  = "surf"

workspaces' :: [String]
workspaces' = zipWith template ([1..9]-:0) $
  [ "root" , "code" , "www" , "mail" , "doc"
  , "srs"  , "?"    , "?"   , "im"   , "mu"
  ] where template number name = [i|#{number}:#{name}|]

findWS :: String -> String
findWS = fromMaybe "NSP" . flip find workspaces' . isSuffixOf

manageHooks :: [ManageHook]
manageHooks =
  [ fullscreenManageHook
  , isFullscreen                          --> doFullFloat
  , isDialog                              --> doCenterFloat
  , className     =? "feh"                --> doCenterFloat
  , className     =? "MPlayer"            --> doCenterFloat
  , title         ?? isPrefixOf "Gnuplot" --> doCenterFloat
  , icon          ?? isInfixOf "gplt"     --> doCenterFloat
  , className     =? "Surf"               --> shiftWS "www"
  , title         ?? isPrefixOf "irssi"   --> shiftWS "im"
  , title         ?? isPrefixOf "mutt"    --> shiftWS "mail"
  , className     =? "Okular"             --> shiftWS "doc"
  , className     =? "Zathura"            --> shiftWS "doc"
  , className     =? ".zathura-wrapped"   --> shiftWS "doc"
  , className     =? "FBReader"           --> shiftWS "doc"
  , name          =? "Mnemosyne"          --> shiftWS "srs"
  , className     =? "Amphetype"          --> shiftWS "type"
  ] where shiftWS = doShift . findWS 

type EventHook = Event -> X All

eventHooks :: [EventHook]
eventHooks = 
  [ fullscreenEventHook
  , minimizeEventHook
  ]

type Layout = ModifiedLayout Minimize (ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest)

layoutHook' :: Monitor -> Layout Word64
layoutHook' = minimize . tabbed shrinkText . themeFromMonitor

themeFromMonitor :: Monitor -> Theme
themeFromMonitor monitor = solarizedTheme
  { fontName   = fontFromMonitor monitor "light"
  , decoHeight = if isRetina monitor then 40 else 29
  }

fontFromMonitor :: Monitor -> String -> String
fontFromMonitor monitor style = show XFTSpecification
  { family = "LetterGothicMono"
  , style  = Just style
  , size   = Just $ if isRetina monitor then 20 else 13
  }

prompt :: XPConfig
prompt = solarizedXPConfig
  { historyFilter       = nub
  , position            = Top
  , showCompletionOnTab = True
  , promptBorderWidth   = 0
  , searchPredicate     = isInfixOf
  , promptKeymap        = ctrlP
  }

promptFromMonitor :: Monitor -> XPConfig
promptFromMonitor monitor = prompt
  { height = if isRetina monitor then 40 else 29
  , font   = fontFromMonitor monitor "light"
  }

ddg :: SearchEngine
ddg = searchEngine "ddg" "http://duckduckgo.com?q="

keys' :: Monitor -> XConfig layout -> Map (KeyMask, KeySym) (X ())
keys' monitor = flip mkKeymap $ 
  [ ( "M-S-l",         spawn "slock"                                                )
  , ( "M-<Tab>",       toggleWS                                                     )
  , ( "M-S-<Return>",  spawn terminal'                                              )
  , ( "M-o",           promptSearchBrowser (promptFromMonitor monitor) browser' ddg )
  , ( "M-S-o",         selectSearchBrowser browser' ddg                             )
  , ( "M-p",           shellPrompt $ promptFromMonitor monitor                      )
  , ( "M-t",           withFocused $ windows . sink                                 )
  , ( "M-S-c",         kill1                                                        )
  , ( "M-m",           withFocused minimizeWindow                                   )
  , ( "M-S-m",         sendMessage RestoreNextMinimizedWin                          )
  , ( "M-j",           windows focusDown                                            )
  , ( "M-k",           windows focusUp                                              )
  ] ++
  [ (m++k, windows $ f w) 
      | (w, k) <- zip workspaces' ("1234567890"??return)
      , (m, f) <- [("M-", greedyView), ("M-S-", shift), ("M-S-C-", copy)]
  ] ++ 
  [ (m++k, screenWorkspace s >>= flip whenJust (windows . f))
      | (k, s) <- zip ("qwer"??return) [0..]
      , (f, m) <- [(view, "M-"), (shift, "M-S-")]
  ]

setFont :: Monitor -> IO ()
setFont (fontFromMonitor -> font) = sequence_ $ 
  [ ("URxvt*font"       , font "light"       )
  , ("URxvt*boldFont"   , font "lightbold"   )
  , ("URxvt*italicFont" , font "lightitalic" )
  ] ?? (setResource . uncurry Resource)

main :: IO ()
main = configureDisplays 
    >> getMonitors ?? maximum 
   >>= liftM2 (>>) setFont (xmonad . configuration)
