#!/bin/bash
#wal_dir=~/ShareDirectory/wallpaper/idol/landscape/karina
#wal_dir=~/ShareDirectory/wallpaper/idol/portrait/karina
wal_dir=~/ShareDirectory/wallpaper/normal

# for wsl
export DISPLAY=192.168.144.1:10.0

# random wallpaper
files=()
while IFS=  read -r -d $'\0'; do
	files+=("$REPLY")
done < <(find $wal_dir -type f -name "*" -print0)
randomfile=$(printf "%s\n" "${files[RANDOM % ${#files[@]}]}")
echo $randomfile
wal -i $randomfile --saturate 1.0


