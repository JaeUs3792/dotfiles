#!/bin/bash

# essential packages
paru -S eza --noconfirm

# clone config
git clone https://github.com/JaeUs3792/dotfiles ~/.dotfiles

paru -S stow --noconfirm
cd ~/.dotfiles
stow .

# korean input
paru -S fcitx5-hangul fcitx5-configtool keyd --noconfirm
sudo mkdir /etc/keyd
sudo cp keyd.default /etc/keyd

sudo systemctl enable --now keyd

# fish
paru -S starship --noconfirm

# file manager
paru -S nemo

# neovim
paru -S neovim nvim-lazy --noconfirm

# git config
git config --global user.name "JaeYoo-Im"
git config --global user.email "cpu3792@gmail.com"
git config --global user.autocrlf input

# tmux
paru -S tmux --noconfirm
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
TMUX_PLUGIN_MANAGER_PATH=~/.config/tmux/plugins/tpm ~/.tmux/plugins/tpm/scripts/install_plugins.sh
TMUX_PLUGIN_MANAGER_PATH=~/.config/tmux/plugins/tpm ~/.tmux/plugins/tpm/bin/update_plugins all

# font
paru -S ttf-firacode-nerd ttf-mononoki-nerd otf-comicshanns-nerd ttf-times-new-roman ttf-nanum noto-fonts-emoji ttf-symbola noto-fonts-cjk --noconfirm

#syncthing
paru -S syncthing --noconfirm
sudo systemctl enable --now syncthing@jaeus.service

# express vpn
paru -S expressvpn-gui
sudo systemctl enable expressvpn
sudo systemctl start expressvpn

#emacs
paru -S emacs --noconfirm
paru -S texlive-basic texlive-langkorean --noconfirm
paru -S jupyterlab --noconfirm
paru -S zathura-pdf-mupdf

#verilator
paru -S verilator

# trilium
paru -S trilium-bin

# sourcegit
paru -S sourcegit-bin

# bspwm
paru -S picom polybar bspwm sxhkd xdotool scrot xclip
