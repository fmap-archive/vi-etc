module XMonad.ManageHook.Query (
  icon,
  name
) where

import XMonad.Core (Query)
import XMonad.ManageHook (stringProperty)

icon :: Query String
icon = stringProperty "WM_ICON_NAME"

name :: Query String
name = stringProperty "WM_NAME"
