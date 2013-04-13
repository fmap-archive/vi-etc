#!/usr/bin/env bash

xrdb -merge .Xdefaults                                      # Load Xdefaults
xsetroot -cursor_name left_ptr                              # Set cursor style
urxvtd -q -f -o &                                           # Initialise URxvt daemon
unclutter -root -idle 1 &                                   # Hide cursor after a second of inactivity.
xsetroot -solid '#fdf6e3'                                   # Solid background
xset -b &
xautolock -time 15 -locker autolock &
xcompmgr &
battlestation "lap" # assumption

echo > ~/.xmonad/history

exec xmonad &>>"$HOME/.xmonad/logs/everything.log"          # Launch XMonad.
