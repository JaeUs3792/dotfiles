#!/bin/sh

# export settings
# cp ~/.config/Code/User/settings.json ~/.dotfiles/scripts/vscode/
# export keybindings
# cp ~/.config/Code/User/keybindings.json ~/.dotfiles/scripts/vscode/


# export extensions
code --list-extensions > ~/.dotfiles/.config/Code/User/extensions.txt

