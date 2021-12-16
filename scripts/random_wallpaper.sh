#!/bin/bash
#wal_dir=~/ShareDirectory/wallpaper/idol/landscape/karina
#wal_dir=~/ShareDirectory/wallpaper/idol/portrait/karina
wal_dir=~/ShareDirectory/wallpaper/normal


export DISPLAY=192.168.144.1:10.0
files=($wal_dir/**/*)
randomfile=$(printf "%s\n" "${files[RANDOM % ${#files[@]}]}")
echo $randomfile
wal -i $randomfile --saturate 1.0
