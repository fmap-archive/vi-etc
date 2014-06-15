#!/usr/bin/env bash

setxkbmap -option 'ctrl:nocaps,compose:rwin'
xkbcomp ~/.Xkeymap $DISPLAY
xrdb -merge .Xdefaults
xsetroot -cursor_name left_ptr
unclutter -root -idle 1 &
xsetroot -solid '#fdf6e3'
xset -b &
ssh-agent $HOME/.xmonad/xmonad

exit 0
