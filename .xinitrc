#!/usr/bin/env bash

/usr/lib/systemd/systemd --user &
systemctl --user start scrutor
systemctl --user start keep
aziz --device display --brightness 300

xrdb -merge .Xdefaults
xsetroot -cursor_name left_ptr
urxvtd -q -f -o &
unclutter -root -idle 1 &
xsetroot -solid '#fdf6e3'
xset -b &
xcompmgr &                    
redshift -l 1.3667:103.8 &
pulseaudio &
bs "lap" # That's _battlestation_

ssh-agent xmonad &>>"$HOME/.xmonad/logs/everything.log"
