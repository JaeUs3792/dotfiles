#!/bin/sh
# Set the screen DPI (uncomment this if needed!)
# xrdb ~/.emacs.d/exwm/Xresources

# Run the screen compositor
#picom &
#picom -b --config /home/jaeus/.config/i3/picom.conf &
# Enable screen locking on suspend
#xss-lock -- slock &
# Fire it up for WSL
#VETHER_IP=$(/bin/grep nameserver /etc/resolv.conf 2> /dev/null | /bin/tr -s ' ' | /bin/cut -d' ' -f2)
#export DISPLAY=$VETHER_IP:10.0
export DISPLAY=192.168.144.1:10.0

picom -b --config $HOME/.xmonad/scripts/picom.conf &
exec dbus-launch --exit-with-session emacs -mm --debug-init -l ~/.emacs.d/desktop.el
