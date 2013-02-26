-- TODO:
-- workspace-scoped window selector prompt?
-- dzen handler library?

import Data.List
import qualified Data.Map
import Data.Ratio
import XMonad
import XMonad.Actions.CopyWindow
import qualified XMonad.Actions.Search as S
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.BoringWindows
import XMonad.Layout.Fullscreen
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.Window
import XMonad.Prompt.Shell
import System.Posix.Unistd
import System.Posix.User
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import qualified XMonad.StackSet as W
import Themes.Solarized
import Util.Display

main :: IO ()
main = do
  bar      <- spawnPipe wsBar
  hostname <- fmap nodeName getSystemID
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
    {  terminal           = terminal'
    ,  workspaces         = workspaces'
    ,  borderWidth        = 1
    ,  normalBorderColor  = base02
    ,  focusedBorderColor = base01
    ,  manageHook         = manageHook'
    ,  handleEventHook    = eventHook'
    ,  layoutHook         = layoutHook'
    ,  startupHook        = startupHook'
    ,  logHook            = logHook' bar
    ,  focusFollowsMouse  = False
    ,  modMask            = if (hostname == "aineko") then mod4Mask else mod1Mask
    ,  keys               = keys'
    }

terminal' = "urxvtc" :: String
browser'  = "surf"   :: String

workspaces' = map (show . (\(a,_) -> a)) $
  [ (1, "code and prose")
  , (2, "web browsing")
  , (3, "irc and mail")
  , (4, "documents: books, papers, whatever")
  , (5, "spaced-repetition")
  , (6, "well, I'm learning to touch-type")
  , (7, "n-back")
  , (8, "free")
  , (9, "music")
  ] 

(displayW,_) = getDisplayDimensions

wsSplitDimensions x = (f x, g x)
  where f = (round . (0.6*) . fromInteger)
        g = (round . (0.4*) . fromInteger)

dzenStyle = " -fn 'Envy Code R:medium:pixelsize=13'" 
         ++ " -bg '" ++ base03 
         ++ "' -fg '" ++ base00 
         ++ "' -e 'onstart=lower'" 

wsBar  = "dzen2 -p -ta l -x " ++ index ++ " -w " ++ width ++  " -h " ++ height ++ dzenStyle
  where (width',_)     = wsSplitDimensions displayW
        [height,index] = map show [22,0]
        width          = show width'

stBar  = "dzen2 -p -ta r -x " ++ index ++ " -w " ++ width ++  " -h " ++ height ++ dzenStyle
  where (_,width')     = wsSplitDimensions displayW
        [height,index] = map show [22, displayW - width']
        width          = show width'

manageHook' :: ManageHook
manageHook' = (composeAll . concat $
  [ [fullscreenManageHook                             ]
  , [isFullscreen                 --> doFullFloat     ]
  , [isDialog                     --> doCenterFloat   ]
  , [className =? "feh"           --> doCenterFloat   ]
  , [className =? "MPlayer"       --> doCenterFloat   ]
  , [titleStarts  "Gnuplot"       --> doCenterFloat   ]
  , [iconHas      "gplt"          --> doCenterFloat   ]
  , [className =? "Xmessage"      --> doResizeFloat   ]
  , [className =? "Surf"          --> doShift "2"     ]
  , [titleStarts  "irssi"         --> doShift "3"     ]
  , [titleStarts  "mutt"          --> doShift "3"     ]
  , [className =? "Okular"        --> doShift "4"     ]
  , [className =? "Mnemosyne"     --> doShift "5"     ]
  , [className =? "Amphetype"     --> doShift "6"     ]
  , [titleStarts "Brain Workshop" --> doShift "7"     ]
  , [titleStarts "Brain Workshop" --> doTile          ]
  ]) <+> namedScratchpadManageHook scratchpads
  where
    name           =  stringProperty "WM_NAME"      :: Query String
    icon           =  stringProperty "WM_ICON_NAME" :: Query String
    titleStarts x  =  fmap (x `isPrefixOf`) title
    iconHas  x     =  fmap (x ==) icon
    doResizeFloat  =  customFloating $ W.RationalRect left top width height
      where
        height  = 2/4
        width   = 2/3
        left    = (/2) $ (1-) width
        top     = (/2) $ (1-) height
    doTile = ask >>= doF . W.sink

scratchpads :: [NamedScratchpad]
scratchpads = concat $
  [ [NS "shell" (terminal "shell" "bash") (resource =? "shell") doSTermLayout ]
  , [NS "ghci"  (terminal "ghci" "ghci")  (resource =? "ghci")  doSTermLayout ]
  , [NS "htop"  (terminal "htop" "htop")  (resource =? "htop")  doSTermLayout ]
  , [NS "dl"    (terminal "dl" "false")   (resource =? "dl")    doSTermLayout ]
  ]
  where 
    terminal name cmd = terminal' ++ " -name " ++ name ++ " -e " ++ cmd
    doSTermLayout = customFloating $ W.RationalRect left top width height
      where
        height  = 2/4
        width   = 2/3
        left    = (/2) $ (1-) width
        top     = (/2) $ (1-) height

startupHook' :: X()
startupHook' = do
  spawn ("~/.xmonad/bin/status.sh | " ++ stBar)
  setWMName "LG3D"

eventHook' = fullscreenEventHook <+> minimizeEventHook

layoutHook' = fullscreenFull                     $
              fullscreenFloat                    $
              avoidStruts                        $
              minimize                           $
              boringWindows                      $
              smartBorders                       $
              onWorkspace "2" tabLayout          $
              onWorkspace "3" mailLayout         $
              onWorkspace "4" tabLayout          $
              onWorkspace "5" (Full)             $
              defaultLayout
  where
    defaultLayout = (Full ||| Tall 1 0.05 tau)
    tau           = toRational $ (2/) $ succ $ sqrt 5
    mailLayout    = (Tall 1 0.05 0.7)
    webLayout     = (Full ||| (Mirror $ mailLayout))
    tabLayout     = (tabbed shrinkText solarizedTheme)

logHook' bar = dynamicLogWithPP $ defaultPP
  { ppOutput          = hPutStrLn bar
  , ppCurrent         = wrap [' '] []
  , ppVisible         = const []
  , ppHidden          = const []
  , ppUrgent          = dzenColor "#dc322f" [] . wrap [' '] []
  , ppSep             = []
  , ppTitle           = pad
  , ppLayout          = dzenColor base01 [] . 
                          (\x -> case x of 
                            "Minimize Tall"            -> dzenXBM "tall.xbm"
                            "Minimize Full"            -> dzenXBM "full.xbm"
                            "Minimize Tabbed Simplest" -> dzenXBM "full.xbm"
                            otherwise                  -> x
                          )
  }
  where
      dzenXBM xbm = wrap " ^i(" ")" $ xbmPath xbm
      xbmPath     = (++) "/home/irving/.xmonad/icons/"

keys' c = mkKeymap c $
  [ ( "M-S-l",         spawn "slock")
  , ( "M-<Return>",    windows W.swapMaster)
  , ( "M-,",           sendMessage (IncMasterN 1))
  , ( "M-.",           sendMessage (IncMasterN (-1)))
  , ( "M-S-<Return>",  spawn terminal')
  , ( "M-b",           sendMessage ToggleStruts)
  , ( "M-o",           S.promptSearchBrowser solarizedXPConfig browser' scrutor)
  , ( "M-S-o",         S.selectSearchBrowser browser' scrutor)
  , ( "M-p",           shellPrompt solarizedXPConfig)
  , ( "M-w",           windowPromptGoto solarizedXPConfig)
  , ( "M-t",           withFocused $ windows . W.sink)
  , ( "M-S-p",         toggleScratchpad "shell")
  , ( "M-S-i",         toggleScratchpad "ghci")
  , ( "M-S-u",         toggleScratchpad "dl")
  , ( "M-S-s",         toggleScratchpad "htop")
  , ( "M-S-c",         kill1) -- s/1// to kill in all workspaces
  , ( "M-m",           withFocused minimizeWindow)
  , ( "M-S-m",         sendMessage RestoreNextMinimizedWin)
  , ( "M-j",           focusDown)
  , ( "M-k",           focusUp)
  , ( "M-S-j",         windows W.swapDown)
  , ( "M-S-k",         windows W.swapUp)
  , ( "M-q",           spawn restart)
  ] ++ -- Function Keys
  [ ("<XF86AudioPlay>",         spawn "mpc toggle")
  , ("<XF86AudioPrev>",         spawn "mpc prev")
  , ("<XF86AudioNext>",         spawn "mpc next")
  , ("<XF86TouchpadToggle>",    spawn "togglemouse")
  , ("<Print>",                 spawn "scrot '%s.png' -e 'mv $f ~/usr/share/scrot/'")
  ] ++
  [ (m++k, windows $ f w) | (w, k) <- zip (XMonad.workspaces c) (map show [1..9])
                          , (m, f) <- [("M-", W.greedyView), ("M-S-", W.shift), ("M-S-C-", copy)]
  ]
  where
    scrutor = S.searchEngine "scrutor" "http://scrutor.aineko/?q="
    toggleScratchpad = (namedScratchpadAction scratchpads)
    restart = "xmonad --recompile &&"
           ++ "killall dzen2 && "
           ++ "xmonad --restart"
