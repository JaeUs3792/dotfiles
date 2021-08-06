#!/bin/sh

echo "===================================="
echo "Share Directory Mount?(y/N)?"
echo ${whoami}
read val
if [ "${val}" == "" ]; then
	val="N"
fi

if [ "${val}" == "y" ];then
	echo "address(ex://192.168.1.144/d)?"
	read addr
	echo "user(ex:JaeUs-HV)?"
	read user
	echo "password?"
	read pass
	echo "mount point(ex:/home/jaeus/mnt)?"
	read mpoint

	sudo echo "${addr} ${mpoint} cifs _netdev,nofail,username=${user},password=${pass},uid=jaeus,gid=jaeus 0 0" >> /etc/fstab
fi

