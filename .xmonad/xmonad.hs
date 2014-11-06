{-# LANGUAGE RecordWildCards, FlexibleContexts, Rank2Types, ViewPatterns, QuasiQuotes, CPP #-}

import Control.Monad (liftM2)
import Data.Functor.Extras ((??))
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, find, nub)
import Data.List.Extras ((-:))
import Data.Map (Map, empty)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(mconcat), All)
import Data.String.Interpolate (i)
import GHC.Word (Word64, Word32)
import Graphics.X11.Dzen2 ()
import Graphics.X11.Monitor (Monitor(..), getMonitors, isRetina, configureDisplays)
import Graphics.X11.Types (Window, KeyMask, KeySym, mod4Mask)
import Graphics.X11.XFT (XFTSpecification(..))
import Graphics.X11.Xlib.Extras (Event)
import Graphics.X11.Xrdb (Resource(..), setResource)
import XMonad.Actions.CopyWindow (copy, kill1)
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.Search.Extras (Browser(..), promptSearchBrowser', selectSearchBrowser', ddg)
import XMonad.Core (X(..), XConfig(..), LayoutClass, ManageHook, spawn, whenJust)
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen, isDialog, doCenterFloat)
import XMonad.Hooks.Minimize (minimizeEventHook)
import XMonad.Layout (Full(..))
import XMonad.Layout.BoringWindows (BoringWindows, boringWindows, focusUp, focusDown)
import XMonad.Layout.Decoration (Decoration, DefaultShrinker)
import XMonad.Layout.Fullscreen (fullscreenEventHook, fullscreenManageHook)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.Minimize (Minimize, minimize, minimizeWindow, MinimizeMsg(RestoreNextMinimizedWin))
import XMonad.Layout.Simplest (Simplest)
import XMonad.Layout.Tabbed (tabbedAlways, TabbedDecoration, Theme(..), shrinkText)
import XMonad.Main (xmonad)
import XMonad.ManageHook ((-->), (=?), className, title, doShift)
import XMonad.ManageHook.Query (icon, name)
import XMonad.Operations (withFocused, windows, sendMessage, screenWorkspace)
import XMonad.Prompt (XPConfig(..), XPPosition(..))
import XMonad.Prompt.Keymap.CtrlP (ctrlP)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Window.CurrentWorkspace (currentWorkspaceWindowPrompt)
import XMonad.StackSet (sink, shift, view, greedyView)
import XMonad.Themes.Solarized (base02, base01, solarizedTheme, solarizedXPConfig)
import XMonad.Util.EZConfig (mkKeymap)
      
configuration :: Monitor -> XConfig Layout
configuration monitor = XConfig
  { terminal           = terminal'
  , workspaces         = workspaces'
  , borderWidth        = 0
  , normalBorderColor  = base02
  , focusedBorderColor = base01
  , manageHook         = mconcat manageHooks
  , handleEventHook    = mconcat eventHooks
  , startupHook        = mconcat $ startupHooks monitor
  , logHook            = return ()
  , focusFollowsMouse  = False
  , layoutHook         = layoutHook' monitor
  , modMask            = mod4Mask
  , keys               = keys' monitor
  , mouseBindings      = const empty
  , clickJustFocuses   = False
  }

terminal' :: String
terminal' = "urxvt"

browserFromMonitor :: Monitor -> Browser
browserFromMonitor monitor = Browser
  { executable = "surf"
  , options    = if isRetina monitor then ["-z","1.8"] else []
  }

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
  , isFullscreen                            --> doFullFloat
  , isDialog                                --> doCenterFloat
  , className     =? "feh"                  --> doCenterFloat
  , className     =? "MPlayer"              --> doCenterFloat
  , title         ?? isPrefixOf "Gnuplot"   --> doCenterFloat
  , icon          ?? isInfixOf "gplt"       --> doCenterFloat
  , className     =? "Surf"                 --> shiftWS "www"
  , title         ?? isPrefixOf "irssi"     --> shiftWS "im"
  , title         ?? isPrefixOf "mutt"      --> shiftWS "mail"
  , className     =? "Okular"               --> shiftWS "doc"
  , className     =? "Zathura"              --> shiftWS "doc"
  , className     =? ".zathura-wrapped"     --> shiftWS "doc"
  , className     =? "FBReader"             --> shiftWS "doc"
  , name          ?? isPrefixOf "Mnemosyne" --> shiftWS "srs"
  , className     =? "Amphetype"            --> shiftWS "type"
  ] where shiftWS = doShift . findWS 

type EventHook = Event -> X All

eventHooks :: [EventHook]
eventHooks = 
  [ fullscreenEventHook
  , minimizeEventHook
  ]

startupHooks :: Monitor -> [X ()]
startupHooks (show.browserFromMonitor->browser) =
  [ spawn "mnemosyne"
  , spawn "urxvt -e mutt -f var/mail/zalora/inbox"
  , spawn "urxvt -e mutt -f var/mail/vikramverma/inbox"
  , spawn "urxvt -e mutt -f var/mail/cuddlecouncil/inbox"
  , spawn [i|net-wait && BROWSER="#{browser}" restore surf|]
  , spawn "restore zathura"
  ]

type Layout = ModifiedLayout Minimize (ModifiedLayout BoringWindows (ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest))

#ifdef i386_HOST_ARCH
layoutHook' :: Monitor -> Layout Word32
#else
layoutHook' :: Monitor -> Layout Word64
#endif
layoutHook' = minimize . boringWindows . tabbedAlways shrinkText . themeFromMonitor

themeFromMonitor :: Monitor -> Theme
themeFromMonitor monitor = solarizedTheme
  { fontName   = fontFromMonitor monitor "light"
  , decoHeight = if isRetina monitor then 40 else 28
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
  { height = if isRetina monitor then 40 else 28
  , font   = fontFromMonitor monitor "light"
  }

keys' :: Monitor -> XConfig layout -> Map (KeyMask, KeySym) (X ())
keys' monitor = flip mkKeymap $ 
  [ ( "M-S-l",         spawn "slock"                            )
  , ( "M-<Tab>",       toggleWS                                 )
  , ( "M-S-<Return>",  spawn terminal'                          )
  , ( "M-<Space>",     currentWorkspaceWindowPrompt prompt      )
  , ( "M-o",           promptSearchBrowser' prompt browser ddg  )
  , ( "M-S-o",         selectSearchBrowser' browser ddg         )
  , ( "M-p",           shellPrompt $ promptFromMonitor monitor  )
  , ( "M-t",           withFocused $ windows . sink             )
  , ( "M-S-c",         kill1                                    )
  , ( "M-m",           withFocused minimizeWindow               )
  , ( "M-S-m",         sendMessage RestoreNextMinimizedWin      )
  , ( "M-j",           focusDown                                )
  , ( "M-k",           focusUp                                  )
  ] ++
  [ (m++k, windows $ f w) 
      | (w, k) <- zip workspaces' ("1234567890"??return)
      , (m, f) <- [("M-", greedyView), ("M-S-", shift), ("M-S-C-", copy)]
  ] ++ 
  [ (m++k, screenWorkspace s >>= flip whenJust (windows . f))
      | (k, s) <- zip ("qwer"??return) [0..]
      , (f, m) <- [(view, "M-"), (shift, "M-S-")]
  ] where (prompt, browser) = (promptFromMonitor monitor, browserFromMonitor monitor)

setFont :: Monitor -> IO ()
setFont monitor = sequence_ $ 
  [ ("URxvt*font"        , fontFromMonitor monitor "light"       )
  , ("URxvt*boldFont"    , fontFromMonitor monitor "lightbold"   )
  , ("URxvt*italicFont"  , fontFromMonitor monitor "lightitalic" )
  , ("URxvt*urlLauncher" , show $ browserFromMonitor monitor     )
  ] ?? (setResource . uncurry Resource)

main :: IO ()
main = configureDisplays 
    >> getMonitors ?? maximum 
   >>= liftM2 (>>) setFont (xmonad . configuration)
