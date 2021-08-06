#!/bin/sh

sudo pacman -S betterlockscreen
sudo pacman -Syu --noconfirm

git clone https://github.com/JaeYoo-Im/myDots ~/.dotfiles
paru -S stow zsh-autosuggestions-git alacritty trayer --noconfirm
/usr/share/oh-my-zsh/tools/install.sh
rm ~/.zshrc ~/.bashrc ~/.xmonad/xmonad.hs
cd ~/.dotfiles
stow .

# neovim
paru -S neovim --noconfirm
sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'

nvim +PlugInstall +q +q

# git config
git config --global user.name "JaeYoo-Im"
git config --global user.email "cpu3792@gmail.com"

# tmux
paru -S tmux --noconfirm
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

TMUX_PLUGIN_MANAGER_PATH=~/.config/tmux/plugins/tpm ~/.tmux/plugins/tpm/scripts/install_plugins.sh
TMUX_PLUGIN_MANAGER_PATH=~/.config/tmux/plugins/tpm ~/.tmux/plugins/tpm/bin/update_plugins all

# Dynamic wallpaper
paru -S cronie --noconfirm
git clone https://github.com/adi1090x/dynamic-wallpaper.git ~/dynamic-wallpaper
cd dynamic-wallpaper
./install.sh

# font
paru -S ttf-fira-code ttf-nanum nerd-fonts-mononoki --noconfirm

# hangul
paru -S fcitx-hangul fcitx-configtool --noconfirm

# enpass
paru -S enpass --noconfirm

paru -S green-tunnel --noconfirm

#emacs
paru -S ripgrep emacs --noconfirm
paru -S auctex texlive-most texlive-lang --noconfirm

#seafile
paru -S seadrive-gui --noconfirm

# etc
paru -S figlet --noconfirm
