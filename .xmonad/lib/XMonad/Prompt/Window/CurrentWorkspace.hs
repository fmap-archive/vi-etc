module XMonad.Prompt.Window.CurrentWorkspace (
  currentWorkspaceWindowPrompt
) where

import Control.Monad (liftM2)
import Data.Functor ((<$>))
import Data.Functor.Extras ((<$$>))
import Data.Maybe (fromMaybe)
import XMonad (X, Window, gets, windowset, windows, whenJust)
import XMonad.StackSet (stack, integrate, current, workspace, focusWindow)
import XMonad.Util.NamedWindows (getName)
import XMonad.Prompt

data CurrentWorkspaceWindowPrompt = CurrentWorkspaceWindowPrompt

instance XPrompt CurrentWorkspaceWindowPrompt where
  showXPrompt       = const "Find window: "
  commandToComplete = const id
  nextCompletion    = const getNextCompletion

getNamedWindows :: X [(String, Window)]
getNamedWindows = fromMaybe [] <$> integrate <$$> stack <$> workspace <$> current <$> gets windowset >>= mapM withName
  where withName = liftM2 fmap (flip (,)) (show <$$> getName)

currentWorkspaceWindowPrompt :: XPConfig -> X ()
currentWorkspaceWindowPrompt xpc = getNamedWindows >>= liftM2 (mkXPrompt CurrentWorkspaceWindowPrompt xpc) (flip cSearch) cAction
  where cSearch pattern = return . filter (searchPredicate xpc pattern) . map fst 
        cAction = flip whenJust (windows . focusWindow) <$$> flip lookup
