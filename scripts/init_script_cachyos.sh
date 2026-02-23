#!/bin/bash

paru -S vivaldi neovim --noconfirm

# clone config
git clone https://github.com/JaeUs3792/dotfiles ~/.dotfiles
rm ~/.config/fish/config.fish
paru -S stow --noconfirm
cd ~/.dotfiles
stow .

# korean input
paru -S fcitx5-hangul fcitx5-configtool fcitx5-gtk keyd --noconfirm
sudo mkdir /etc/keyd
sudo cp keyd.default /etc/keyd

sudo systemctl enable --now keyd

# add ntfs support
paru -S ntfs-3g --noconfirm

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
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
~/.config/emacs/bin/doom install
paru -S texlive-basic texlive-langkorean --noconfirm
paru -S jupyterlab --noconfirm
paru -S zathura-pdf-mupdf

#verilator
paru -S verilator

# sourcegit
paru -S sourcegit-bin

# bspwm
paru -S picom polybar bspwm sxhkd xdotool scrot xclip rofi
# theme
paru -S arc-gtk-theme papirus-icon-theme python-pywal

# related applet
paru -S network-manager-applet
paru -S i3lock-color

# scratchpad (wezterm, xdo) & lock screen blur (imagemagick)
paru -S wezterm xdo imagemagick --noconfirm

# file manager
paru -S thunar gvfs gvfs-smb tumbler --noconfirm

# yazi (terminal file manager)
paru -S yazi ffmpegthumbnailer unar jq poppler fd ripgrep fzf zoxide plocate chafa --noconfirm
sudo systemctl enable --now plocate-updatedb.timer

# plocate: /home(btrfs 서브볼륨) 인덱싱 허용, USB(/run/media) 제외
sudo sed -i 's/PRUNE_BIND_MOUNTS = "yes"/PRUNE_BIND_MOUNTS = "no"/' /etc/updatedb.conf
sudo sed -i 's|PRUNEPATHS = "\(.*\)"|PRUNEPATHS = "\1 /run/media"|' /etc/updatedb.conf
sudo updatedb
# bspwm utils
paru -S wmname xsettingsd volumeicon --noconfirm
cp /usr/share/applications/vivaldi-stable.desktop ~/.local/share/applications/
sed -i 's|Exec=/usr/bin/vivaldi-stable|Exec=/usr/bin/vivaldi-stable --password-store=kwallet6|g' ~/
.local/share/applications/vivaldi-stable.desktop
update-desktop-database ~/.local/share/applications/
