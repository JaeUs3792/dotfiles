#!/bin/bash

#Input Directory
input_dir=/mnt/wsl/PHYSICALDRIVE1/input/WAAA
output_dir=~/Downloads/input

walk_dir () {
    shopt -s nullglob dotglob

    for pathname in "$1"/*; do
        if [ -d "$pathname" ]; then
            walk_dir "$pathname"
        else
            #printf '%s\n' "$pathname"
			base_name=$(basename ${pathname}) # only filename
            #printf '%s, %s\n' "$base_name" "$output_dir/$base_name"
			HandBrakeCLI -i $pathname -o $output_dir/$base_name -Z "General/Very Fast 480p30" --no-two-pass -b 500
        fi
    done
}

DOWNLOADING_DIR=$input_dir

walk_dir "$DOWNLOADING_DIR"


