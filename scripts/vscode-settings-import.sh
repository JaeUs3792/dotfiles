#!/bin/sh

# import extensions
while read extension; do
    #code --install-extension "$extension"
    echo $extension
done < ~/.dotfiles/scripts/vscode/extensions.txt
