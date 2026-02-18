#!/bin/bash
#wal_dir=~/walls/idol/landscape/karina
#wal_dir=~/ShareDirectory/wallpaper/idol/portrait/karina
wal_dir=~/walls-19

# random wallpaper
files=()
while IFS= read -r -d $'\0'; do
    files+=("$REPLY")
done < <(find $wal_dir -type f -name "*" -print0)
randomfile=$(printf "%s\n" "${files[RANDOM % ${#files[@]}]}")
echo $randomfile
feh --bg-max "$randomfile"
