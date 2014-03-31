#!/usr/bin/env bash

aziz --device display --brightness 300
setxkbmap -option 'ctrl:nocaps,compose:rwin'
xkbcomp ~/.Xkeymap $DISPLAY
(lsusb | grep -qi wacom) && worientation half
xrdb -merge .Xdefaults
xsetroot -cursor_name left_ptr
urxvtd -q -f -o &
unclutter -root -idle 1 &
xsetroot -solid '#fdf6e3'
xset -b &
xcompmgr &                    
redshift -l 1.3667:103.8 &
restore zathura &
net-wait && restore surf &
net-wait && ssh-wait && sshfs -o nonempty tau:/media/ ~/usr/share/ & 
bs 

ssh-agent $HOME/.xmonad/xmonad &>>"$HOME/.xmonad/logs/everything.log"

exit 0
