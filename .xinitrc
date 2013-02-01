#!/usr/bin/env bash

xrandr --output Virtual1 --mode 1366x768 &
setxkbmap -option && setxkbmap -option caps:escape          # Disable caps-lock, remap the key to escape
xrdb -merge .Xdefaults                                      # Load Xdefaults
xsetroot -cursor_name left_ptr                              # Set cursor style
urxvtd -q -f -o &                                           # Initialise URxvt daemon
unclutter -root -idle 1 &                                   # Hide cursor after a second of inactivity.
xsetroot -solid '#fdf6e3'                                   # Solid background
feh --bg-scale ~/usr/share/wallpapers/sleep_of_reason.jpg & # wallpaper
xset -b &
xautolock -time 15 -locker autolock &

echo > ~/.xmonad/history

exec xmonad &>>"$HOME/.xmonad/logs/everything.log"          # Launch XMonad.
