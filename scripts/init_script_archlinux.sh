#!/bin/bash

# essential packages
yay -S eza --noconfirm

# clone config
git clone https://github.com/JaeUs3792/dotfiles ~/.dotfiles

yay -S stow --noconfirm
cd ~/.dotfiles
stow .

# korean input
yay -S ibus-hangul

#ZSH
yay -S zsh starship --noconfirm
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ~/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting
git clone https://github.com/zsh-users/zsh-autosuggestions.git ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions
mv ~/.zshrc.pre-oh-my-zsh ~/.zshrc

# neovim
yay -S neovim --noconfirm
sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
nvim +PlugInstall +q +q

# git config
git config --global user.name "JaeYoo-Im"
git config --global user.email "cpu3792@gmail.com"
git config --global user.autocrlf input

# tmux
yay -S tmux --noconfirm
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
TMUX_PLUGIN_MANAGER_PATH=~/.config/tmux/plugins/tpm ~/.tmux/plugins/tpm/scripts/install_plugins.sh
TMUX_PLUGIN_MANAGER_PATH=~/.config/tmux/plugins/tpm ~/.tmux/plugins/tpm/bin/update_plugins all

# font
yay -S ttf-firacode-nerd ttf-momonoki-nerd otf-comicshanns-nerd ttf-times-new-roman ttf-nanum noto-fonts-emoji ttf-symbola noto-font-cjk --noconfirm

#syncthing
yay -S syncthing --noconfirm
sudo systemctl enable syncthing@jaeus.service
sudo systemctl start syncthing@jaeus.service

#emacs
yay -S emacs --noconfirm
paru -S texlive-basic texlive-langkorean --noconfirm
paru -S jupyterlab --noconfirm
paru -S zathura-pdf-mupdf

#verilator
yay -S verilator

# trilium
yay -S trilium-bin
