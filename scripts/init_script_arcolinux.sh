#!/bin/sh
paru
paru -S stow trayer --noconfirm

# config file handling
rm ~/.config/polybar/config
rm ~/.zshrc ~/.bashrc ~/.xmonad/xmonad.hs
rm -rf ~/.config/alacritty ~/.xmonad/scripts
cd ~/.dotfiles
stow .


#ZSH
paru -S zsh-autosuggestions-git --noconfirm
/usr/share/oh-my-zsh/tools/install.sh
mv ~/.zshrc.pre-oh-my-zsh ~/.zshrc

# NUSHELL
#paru -S nushell
#nu ~/scripts/init_script_nushell.nu
#echo "Changing Shell to nushell"
#chsh -s /bin/nu   # change shell

# FISH
#paru -S fish starship
#chsh -s /bin/fish

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
paru -S ttf-fira-code ttf-nanum nerd-fonts-mononoki ttf-monaco ttf-d2coding --noconfirm

# hangul
paru -S fcitx-hangul fcitx-configtool --noconfirm

# enpass
paru -S enpass --noconfirm

#paru -S green-tunnel --noconfirm

#emacs
paru -S ripgrep emacs --noconfirm
paru -S auctex texlive-most texlive-lang --noconfirm
paru -S jupyter --noconfirm

#seafile
paru -S seadrive-gui --noconfirm

#syncthing
paru -S syncthing --noconfirm
mkdir SyncThing

sudo systemctl enable syncthing@jaeus.service

# etc
paru -S figlet --noconfirm

