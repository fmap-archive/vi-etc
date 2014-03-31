#!/usr/bin/env bash

print() {
  echo -n "$@ • "
}

time::hm() {
  date '+%H:%M'
}
  
time::fuzzy()  {
  fdate
}

_time() {
  time::fuzzy
}

battery::charging() {
  grep -qiP '(?<!dis)charging' /sys/class/power_supply/BAT0/status
}

battery::full() {
  grep -qiP '(?<!dis)charging' /sys/class/power_supply/BAT0/status
}

battery::capacity() {
  cat /sys/class/power_supply/BAT0/capacity
} 

battery() {
  if ! `battery::charging`; then
    if [ `battery::capacity` -lt 20 ]; then 
      print '^fg(#dc322f)dying^fg()'
    fi
  fi 
}

network::connected() {
  grep -qE 'wlan|eth|ath|wlp|enp' /proc/net/route 
}

network() {
  network::connected || 
    print 'offline' # no internet
}

audio::volume() { 
  amixer get Master | grep -Po '\d+%'
}

volume(){
  loud=false
  for channel in `audio::volume | sort -u`; do
    [[ ${channel%\%} -gt 60 ]] && loud=true 
  done; $loud && print '^fg(#dc322f) screaming^fg()'
}

while true; do
  (volume;network;battery;_time) | sed 's/ • $/\n/'
  sleep 5
done
