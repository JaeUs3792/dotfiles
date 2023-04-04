#!/bin/bash

function run {
  if ! pgrep $1 ;
  then
    $@&
  fi
}

#(sleep 2; run $HOME/.config/polybar/launch.sh) &

# cursor
xsetroot -cursor_name left_ptr &

# background
#feh --bg-fill /usr/share/backgrounds/arcolinux/arco-wallpaper.jpg &
#dwall -p -s firewatch
source ~/scripts/wsl_get_display.sh
~/scripts/random_wallpaper.sh

# other apps
#run variety &
#run nm-applet &
#run pamac-tray &
#run xfce4-power-manager &
#run volumeicon &
#run bitwarden-desktop &
#numlockx on &
#blueberry-tray &
#run lxsession &

#fcitx &
picom -b --config $HOME/.xmonad/scripts/picom.conf &
#/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
#/usr/lib/xfce4/notifyd/xfce4-notifyd &
#seadrive-gui &

#emacs --daemon &

#sleep 2
#trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 0 --transparent true --alpha 0 --tint 0x282c34  --height 16 &
