import Data.List
import Data.Char
import qualified Data.Map
import Data.Ratio
import Data.Monoid

import XMonad
import qualified XMonad.Actions.Search as S
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowGo
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
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import qualified XMonad.StackSet as W

import Themes.Solarized
import Util.Display
import Util.Dzen2
import Util.XFT
import qualified Prompt.Keymap.CtrlP as Keymap

main :: IO ()
main = do
  bar <- spawnPipe wsBar
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
    {  terminal           = terminal'
    ,  workspaces         = workspaces'
    ,  borderWidth        = 4
    ,  normalBorderColor  = base02
    ,  focusedBorderColor = base01
    ,  manageHook         = manageHook'
    ,  handleEventHook    = eventHook'
    ,  layoutHook         = layoutHook'
    ,  startupHook        = startupHook'
    ,  logHook            = logHook' bar
    ,  focusFollowsMouse  = False
    ,  modMask            = mod4Mask
    ,  keys               = keys'
    }

terminal' = "urxvtc" :: String
browser'  = "surf"   :: String

workspaces' :: [String]
workspaces' = map (\(n,w) -> mconcat [show n,":",w]) $
  [ (1, "root")
  , (2, "work")
  , (3, "www" )
  , (4, "mail")
  , (5, "doc" )
  , (6, "srs" )
  , (7, "?"   )
  , (8, "?"   )
  , (9, "im"  )
  , (0, "mu"  )
  ] 

findWS :: String -> String
findWS = maybe "NSP" id . flip find workspaces' . isSuffixOf

displayIndex :: Int
displayIndex = 1

(displayW,_) = getDisplayDimensions index
  where index = pred displayIndex

wsSplitDimensions x = (f x, g x)
  where f = round . (0.6*) . fromIntegral
        g = round . (0.4*) . fromIntegral

font' :: String
font' = show XFTSpecification 
  { family = "LetterGothicMono" 
  , style  = Just "Light" 
  , size   = Just 13
  }

dzenStyle :: Dzen2Config
dzenStyle = solarizedDzenConfig
  { fn = Just font'
  , e  = Just [onStartLower]
  } where onStartLower = Event "onstart" [Action "lower" []]

wsBar :: String
wsBar = dzen2Config $ dzenStyle
  { p  = Right Nothing
  , ta = Just LeftA
  , x  = Just index
  , w  = Just width
  , h  = Just height
  , xs = Just displayIndex
  } where (width,_) = wsSplitDimensions displayW
          height = 29
          index = 0

stBar :: String
stBar = dzen2Config $ dzenStyle
  { p = Right Nothing
  , ta = Just RightA
  , x = Just index
  , w = Just width
  , h = Just height
  , xs = Just displayIndex
  } where (_,width) = wsSplitDimensions displayW
          height = 29
          index = displayW - width

manageHook' :: ManageHook
manageHook' = (composeAll . concat $
  [ [fullscreenManageHook                                     ]
  , [isFullscreen                 --> doFullFloat             ]
  , [isDialog                     --> doCenterFloat           ]
  , [className =? "feh"           --> doCenterFloat           ]
  , [className =? "MPlayer"       --> doCenterFloat           ]
  , [titleStarts  "Gnuplot"       --> doCenterFloat           ]
  , [iconHas      "gplt"          --> doCenterFloat           ]
  , [className =? "Xmessage"      --> doResizeFloat           ]
  , [className =? "Surf"          --> doShift (findWS "www")  ]
  , [titleStarts  "irssi"         --> doShift (findWS "im")   ]
  , [titleStarts  "mutt"          --> doShift (findWS "mail") ]
  , [className =? "Okular"        --> doShift (findWS "doc")  ]
  , [className =? "Zathura"       --> doShift (findWS "doc")  ]
  , [className =? "FBReader"      --> doShift (findWS "doc")  ]
  , [className =? "Mnemosyne"     --> doShift (findWS "srs")  ]
  , [titleStarts  "load_ssh_keys" --> doSTermLayout           ]
  ]) <+> namedScratchpadManageHook scratchpads
  where name           =  stringProperty "WM_NAME"      :: Query String
        icon           =  stringProperty "WM_ICON_NAME" :: Query String
        iconHas  x     =  fmap (x ==) icon
        doTile = ask >>= doF . W.sink
        
doResizeFloat  =  customFloating $ W.RationalRect left top width height
  where height  = 2/4
        width   = 2/3
        left    = (/2) $ (1-) width
        top     = (/2) $ (1-) height
        
titleStarts x  = fmap (x `isPrefixOf`) title

scratchpads :: [NamedScratchpad]
scratchpads = concat $ 
  [ [ nst "shell" "bash"      ]
  , [ nst "ghci" "ghci -v0"   ]
  , [ nst "js" "js17"         ]
  , [ nst "ruby" "irb"        ]
  , [ nst "htop" "htop"       ]
  , [ nst "notes" noteCommand ]
  , [ nst "dl" "false"        ]
  ] where nst name cmd = NS name (newTerm name cmd) (resource =? name) doSTermLayout
          noteCommand = "vim -c 'cd ~/root/notes' ~/root/notes/scratchpad.txt"

newTerm :: String -> String -> String
newTerm title cmd = terminal' ++ " -T " ++ title ++ " -name " ++ title ++ " -e " ++ cmd

doSTermLayout = customFloating $ W.RationalRect left top width height
  where height  = 3/5
        width   = 4/5
        left    = (/2) $ (1-) width
        top     = (/2) $ (1-) height

startupHook' :: X ()
startupHook' = do
  spawn $ "bash -c ~/.xmonad/scripts/status.sh | " ++ stBar
  spawn $ newTerm "load_ssh_keys" "load_ssh_keys"
  (newTerm "mutt" "mutt") `runIfNot` (title =? "mutt")
  "mnemosyne" `runIfNot` (className =? "Mnemosyne")
  ("ssh-wait && net-wait &&"++newTerm "irssi" "ssh tau -t tmux a") `shIfNot` titleStarts "irssi"
  setWMName "LG3D"

runIfNot :: String -> Query Bool -> X ()
runIfNot cmd qry = ifWindow qry idHook $ spawn cmd

shIfNot :: String -> Query Bool -> X ()
shIfNot cmd qry = ifWindow qry idHook $ spawn $ sh cmd
  where sh x = "sh -c '"<>x<>"'"

eventHook' = fullscreenEventHook <+> minimizeEventHook

layoutHook' = fullscreenFull                         $
              fullscreenFloat                        $
              avoidStruts                            $
              minimize                               $
              boringWindows                          $
              smartBorders                           $
              onWorkspace (findWS "www") tabLayout   $
              onWorkspace (findWS "doc") tabLayout   $
              defaultLayout
  where
    defaultLayout = Full ||| Tall 1 0.05 tau
    tau           = toRational . (2/) . succ . sqrt $ 5
    tabLayout     = tabbed shrinkText theme

logHook' bar = dynamicLogWithPP $ defaultPP
  { ppOutput          = hPutStrLn bar
  , ppCurrent         = (:) ' '
  , ppVisible         = const []
  , ppHidden          = const []
  , ppUrgent          = dzenColor "#dc322f" ""
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
  where dzenXBM xbm = wrap " ^i(" ")" $ xbmPath xbm
        xbmPath     = (++) "/home/vi/.xmonad/icons/"

keys' c = mkKeymap c $
  [ ( "M-S-l",         spawn "slock")
  , ( "M-<Return>",    windows W.swapMaster)
  , ( "M-<Tab>",       toggleWS' ["NSP"])
  , ( "M-,",           sendMessage $ IncMasterN 1)
  , ( "M-.",           sendMessage $ IncMasterN (-1))
  , ( "M-S-<Return>",  spawn terminal')
  , ( "M-b",           sendMessage ToggleStruts)
  , ( "M-o",           S.promptSearchBrowser promptConfig browser' scrutor)
  , ( "M-S-o",         S.selectSearchBrowser browser' scrutor)
  , ( "M-p",           shellPrompt promptConfig)
  , ( "M-<Space>",     windowPromptGoto noTabPromptConfig)
  , ( "M-t",           withFocused $ windows . W.sink)
  , ( "M-S-p",         toggleScratchpad "shell")
  , ( "M-S-i",         toggleScratchpad "ghci")
  , ( "M-S-u",         toggleScratchpad "dl")
  , ( "M-S-s",         toggleScratchpad "htop")
  , ( "M-S-n",         toggleScratchpad "notes")
  , ( "M-r",           toggleScratchpad "ruby")
  , ( "M-n",           toggleScratchpad "js")
  , ( "M-S-c",         kill1) -- s/1// to kill in all workspaces
  , ( "M-m",           withFocused minimizeWindow)
  , ( "M-S-m",         sendMessage RestoreNextMinimizedWin)
  , ( "M-j",           focusDown)
  , ( "M-k",           focusUp)
  , ( "M-S-j",         windows W.swapDown)
  , ( "M-S-k",         windows W.swapUp)
  , ( "M-q",           spawn restart)
  , ( "<F3>",          sendMessage NextLayout)
  , ( "M-h",           sendMessage Shrink)
  , ( "M-l",           sendMessage Expand)
  ] ++ -- Function Keys
  [ (m++k, windows $ f w) 
        | (w, k) <- zip (XMonad.workspaces c) $ map show $ [1..9]++[0]
        , (m, f) <- [("M-", W.greedyView), ("M-S-", W.shift), ("M-S-C-", copy)]
  ] ++ 
  [ (m++k, screenWorkspace s >>= flip whenJust (windows . f))
        | (k, s) <- zip ["w","e"] [0..] -- More displays? Describe more keys
        , (f, m) <- [(W.view, "M-"), (W.shift, "M-S-")]
  ]
  where
    --scrutor = S.searchEngine "ddg" "https://duckduckgo.com/?kp=-1&kz=-1&kf=-1&kg=p&ks=m&kw=s&km=l&ku=1&ko=-1&k4=-1&ke=-1&kk=s&kr=-1&kq=-1&k1=-1&kx=r&q="
    scrutor = S.searchEngine "scrutor" "http://scrutor.aineko/?q="
    toggleScratchpad = (namedScratchpadAction scratchpads)
    restart = "xmonad --recompile &&"
           ++ "bs"

theme :: Theme
theme = solarizedTheme
  { fontName = "xft:"++font'
  , decoHeight = 29
  }

promptConfig :: XPConfig
promptConfig = solarizedXPConfig
  { height        = 29
  , font          = "xft:"++font'
  , historyFilter = nub
  , position      = Top
  , showCompletionOnTab = True
  , promptBorderWidth = 0
  , searchPredicate = isInfixOf
  , promptKeymap    = Keymap.ctrlP
  }

noTabPromptConfig = promptConfig
  { showCompletionOnTab = False
  --, historyFilter = (filter (not.("NSP"`isInfixOf`)).nub)
  }
