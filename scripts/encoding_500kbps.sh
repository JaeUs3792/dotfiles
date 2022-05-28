#!/bin/bash

#Input Directory
input_dir=$1 #~/mnt/EAST/Aimi\ Rei
output_dir=~/Downloads/input

# get bitrate


walk_dir () {
    shopt -s nullglob dotglob

    for pathname in "$1"/*; do
        if [ -d "$pathname" ]; then
            walk_dir "$pathname"
        else
            #printf '%s\n' "$pathname"
            base_name=$(basename "${pathname}") # only filename
            if [[ "$base_name" == *mp4 ]]; then
                brate=$(mediainfo --Output="Video;%BitRate%" "$pathname")
                brate_int=$((brate))
                printf "$base_name 's bit rate is $brate_int\n"
                if [ $brate_int -gt 550000 ]; then
                    HandBrakeCLI -i "$pathname" -o $output_dir/$base_name -Z "General/Very Fast 480p30" --no-two-pass -b 500
                fi
            fi
            #printf '%s, %s\n' "$base_name" "$output_dir/$base_name"
            #HandBrakeCLI -i $pathname -o $output_dir/$base_name -Z "General/Very Fast 480p30" --no-two-pass -b 500
        fi
    done
}

DOWNLOADING_DIR=$input_dir

walk_dir "$DOWNLOADING_DIR"


