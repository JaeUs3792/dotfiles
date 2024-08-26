#!/bin/bash
if [ "$#" -ne 3 ]; then
	echo "Usage : ./merge_all_files.sh [dir] [pattern] [file]"
	exit
fi

pushd $1
find . -type f -name "*.$2" -exec sh -c 'cat "{}"' >> $3 \;
popd



# files=()
# while IFS=  read -r -d $'\0'; do
#     files+=("$REPLY")
# done < <(find . -type f -name "*.$1" -print0)

# echo $files
# for file in files; do
# 	echo $file
# done
