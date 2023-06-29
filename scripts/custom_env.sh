#!/bin/bash

# configurable parameter
JAEUS_ENV="WSL"

if [[ $JAEUS_ENV == "WSL" ]];then
	VETHER_IP=$(/bin/grep nameserver /etc/resolv.conf 2> /dev/null | /bin/tr -s ' ' | /bin/cut -d' ' -f2)
	export DISPLAY=$VETHER_IP:10.0
fi
