#!/usr/bin/env bash

datetime() {
  ruby "/home/$USER/.xmonad/bin/fuzzy"
}

battery() {
  if grep -q "off-line" <(acpi -V); then
    if [[ $(acpi | awk '{print $4}' | sed 's/%,//') -lt 20 ]]; then
      echo -n '^fg(#dc322f)'
    fi
    echo -n 'battery ^fg()• '
  fi 
}

network() {
  grep -q -E 'wlan|eth|ath' /proc/net/route ||
    echo -n 'offline • '
}

vpn() {
  VPN="$HOME/bin/vpn"
  if grep -q "session established" <($VPN status); then
    echo -n 'vpn • '
  fi
}

while true; do
  vpn&&network&&battery&&datetime
  sleep 20
done
