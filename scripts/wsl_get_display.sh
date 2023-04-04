#!/bin/sh
VETHER_IP=$(/bin/grep nameserver /etc/resolv.conf 2> /dev/null | /bin/tr -s ' ' | /bin/cut -d' ' -f2)
export DISPLAY=$VETHER_IP:10.0
