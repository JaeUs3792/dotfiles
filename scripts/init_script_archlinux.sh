#!/bin/bash

# essential packages
paru -S eza --noconfirm

# clone config
git clone https://github.com/JaeUs3792/dotfiles ~/.dotfiles

paru -S stow --noconfirm
cd ~/.dotfiles
stow .

# korean input
paru -S ibus-hangul

#ZSH
paru -S zsh starship --noconfirm
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ~/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting
git clone https://github.com/zsh-users/zsh-autosuggestions.git ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions
mv ~/.zshrc.pre-oh-my-zsh ~/.zshrc

# fish
paru -S fish starship autin bat --noconfirm


# neovim
paru -S neovim --noconfirm
sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
nvim +PlugInstall +q +q

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
sudo systemctl enable syncthing@jaeus.service
sudo systemctl start syncthing@jaeus.service

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
