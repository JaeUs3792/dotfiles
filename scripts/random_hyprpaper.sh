#!/bin/bash
wal_dir1=~/walls/normal
wal_dir2=~/walls/normal

# random wallpaper
files=()
while IFS=  read -r -d $'\0'; do
    files+=("$REPLY")
done < <(find $wal_dir1 -type f -name "*" -print0)
randomfile=$(printf "%s\n" "${files[RANDOM % ${#files[@]}]}")
echo $randomfile

hyprctl hyprpaper preload $randomfile
hyprctl hyprpaper wallpaper "DP-3,$randomfile"

files=()
while IFS=  read -r -d $'\0'; do
    files+=("$REPLY")
done < <(find $wal_dir2 -type f -name "*" -print0)
randomfile=$(printf "%s\n" "${files[RANDOM % ${#files[@]}]}")
echo $randomfile
hyprctl hyprpaper preload $randomfile
hyprctl hyprpaper wallpaper "HDMI-A-1,$randomfile"

