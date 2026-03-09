#!/bin/bash

paru -S vivaldi neovim starship --noconfirm

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

# zellij
paru -S zellij --noconfirm
mkdir -p ~/.config/zellij/plugins
curl -L https://github.com/fresh2dev/zellij-autolock/releases/download/0.2.2/zellij-autolock.wasm \
    -o ~/.config/zellij/plugins/zellij-autolock.wasm

# font
paru -S ttf-firacode-nerd ttf-mononoki-nerd otf-comicshanns-nerd ttf-times-new-roman ttf-nanum ttf-symbola otf-bebas-neue-git ttf-jetbrains-mono ttf-font-awesome --noconfirm

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
paru -S texlive-basic texlive-langkorean texlive-latex texlive-latexextra texlive-latexrecommended texlive-pictures texlive-binextra texlive-plaingeneric texlive-langcjk texlive-xetex --noconfirm
paru -S jupyterlab --noconfirm
paru -S zathura-pdf-mupdf

# distrobox (podman 설치 시 OCI runtime: crun 선택)
paru -S distrobox podman
echo 'container_default_shell="/bin/bash"' >> ~/.distroboxrc

#verilator
paru -S verilator

# verilog lsp
paru -S verible-bin

# sourcegit
paru -S sourcegit-bin

# bspwm
paru -S picom polybar bspwm sxhkd xdotool scrot xclip rofi wmname
paru -S eww
# theme
paru -S arc-gtk-theme papirus-icon-theme python-pywal

# related applet
paru -S network-manager-applet
paru -S i3lock-color

# scratchpad (ghostty, xdo) & lock screen blur (imagemagick)
paru -S ghostty xdo imagemagick --noconfirm

# network tools
paru -S nmap --noconfirm

# samba
paru -S samba --noconfirm
sudo cp /etc/samba/smb.conf /etc/samba/smb.conf.bak
sudo tee /etc/samba/smb.conf > /dev/null <<'EOF'
[global]
workgroup = WORKGROUP
server string = CachyOS Samba
security = user
map to guest = Bad Password
server min protocol = SMB2
server max protocol = SMB3

[bin_history]
path = /home/jaeus/works/mwtm_bootgen/bin_history
browseable = yes
read only = no
writable = yes
guest ok = no
valid users = user
EOF
sudo systemctl enable --now smb
sudo ufw allow 139,445/tcp

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
cp /usr/share/applications/vivaldi-stable.desktop ~/.local/share/applications/
sed -i 's|Exec=/usr/bin/vivaldi-stable|Exec=/usr/bin/vivaldi-stable --password-store=kwallet6|g' ~/
.local/share/applications/vivaldi-stable.desktop
update-desktop-database ~/.local/share/applications/

# Thunar: ghostty terminal with working directory support
mkdir -p ~/.config/xfce4
echo -e "[Default]\nTerminalEmulator=ghostty" > ~/.config/xfce4/helpers.rc
sed -i 's|<command>exo-open --working-directory %f --launch TerminalEmulator</command>|<command>ghostty --working-directory=%f -e fish</command>|' ~/.config/Thunar/uca.xml
