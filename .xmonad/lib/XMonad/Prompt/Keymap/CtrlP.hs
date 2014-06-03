module XMonad.Prompt.Keymap.CtrlP (ctrlP) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Prompt
import Data.Char (isSpace)
import qualified Data.Map as M
import Control.Arrow (first)

ctrlP :: M.Map (KeyMask,KeySym) (XP ())
ctrlP = ctrlP' isSpace

ctrlP' :: (Char -> Bool) -> M.Map (KeyMask,KeySym) (XP ())
ctrlP' p = M.fromList $
  map (first $ (,) controlMask) -- control + <key>
  [ (xK_u, killBefore)
  , (xK_k, killAfter)
  , (xK_a, startOfLine)
  , (xK_e, endOfLine)
  , (xK_p, pasteString)
  , (xK_Right, moveWord' p Next)
  , (xK_Left, moveWord' p Prev)
  , (xK_Delete, killWord' p Next)
  , (xK_BackSpace, killWord' p Prev)
  , (xK_w, killWord' p Prev)
  , (xK_g, quit)
  , (xK_bracketleft, quit)
  , (xK_Escape, quit)
  ] ++
  map (first $ (,) 0)
  [ (xK_Return, setSuccess True >> setDone True)
  , (xK_KP_Enter, setSuccess True >> setDone True)
  , (xK_BackSpace, deleteString Prev)
  , (xK_Delete, deleteString Next)
  , (xK_Left, moveCursor Prev)
  , (xK_Right, moveCursor Next)
  , (xK_Home, startOfLine)
  , (xK_End, endOfLine)
  , (xK_Down, moveHistory W.focusUp')
  , (xK_Up, moveHistory W.focusDown')
  , (xK_Escape, quit)
  ]
