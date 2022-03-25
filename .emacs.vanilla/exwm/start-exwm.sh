#!/bin/sh
# Set the screen DPI (uncomment this if needed!)
# xrdb ~/.emacs.d/exwm/Xresources

# Run the screen compositor
#picom &
#picom -b --config /home/jaeus/.config/i3/picom.conf &
# Enable screen locking on suspend
#xss-lock -- slock &

rm ~/.emacs.d/emacs.el
picom -b --config $HOME/.xmonad/scripts/picom.conf &
~/scripts/random_wallpaper.sh
exec dbus-launch --exit-with-session emacs --with-profile vanilla -mm --debug-init -l ~/.emacs.d/desktop.el