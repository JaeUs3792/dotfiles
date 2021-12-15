#!/bin/bash

files=($1/*)
randomfile=$(printf "%s\n" "${files[RANDOM % ${#files[@]}]}")
echo $randomfile
wal -i $randomfile --saturate 1.0
