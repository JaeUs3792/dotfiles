#!/bin/bash

sudo apt update && sudo apt upgrade -y
sudo apt install -y git stow curl wget build-essential

# git config
git config --global user.name "JaeYoo-Im"
git config --global user.email "cpu3792@gmail.com"
git config --global user.autocrlf input

# clone dotfiles
git clone https://github.com/JaeUs3792/dotfiles ~/.dotfiles
cd ~/.dotfiles
stow .

# NTFS support
sudo apt install -y ntfs-3g

# Rust toolchain (needed for cargo installs)
sudo apt install -y rustup
rustup toolchain install stable
source "$HOME/.cargo/env"

# Fish + starship
sudo apt install -y fish
curl -sS https://starship.rs/install.sh | sh

# Korean input (fcitx5) - fcitx5-qt: Qt/KDE 앱 입력 통합
sudo apt install -y fcitx5 fcitx5-hangul fcitx5-config-qt fcitx5-qt

# keyd - build from source
sudo apt install -y libevdev-dev libudev-dev
mkdir -p ~/works
git clone https://github.com/rvaiya/keyd ~/works/keyd
cd ~/works/keyd
make && sudo make install
sudo mkdir -p /etc/keyd
sudo cp ~/.dotfiles/scripts/keyd.default /etc/keyd/default.conf
sudo systemctl enable --now keyd

# Vivaldi
wget -qO- https://repo.vivaldi.com/archive/linux_signing_key.pub \
    | sudo gpg --dearmor -o /usr/share/keyrings/vivaldi-keyring.gpg
echo "deb [signed-by=/usr/share/keyrings/vivaldi-keyring.gpg arch=$(dpkg --print-architecture)] https://repo.vivaldi.com/archive/deb/ stable main" \
    | sudo tee /etc/apt/sources.list.d/vivaldi.list
sudo apt update && sudo apt install -y vivaldi-stable
mkdir -p ~/.local/share/applications
cp /usr/share/applications/vivaldi-stable.desktop ~/.local/share/applications/
sed -i 's|Exec=/usr/bin/vivaldi-stable|Exec=/usr/bin/vivaldi-stable --password-store=kwallet6|g' \
    ~/.local/share/applications/vivaldi-stable.desktop
update-desktop-database ~/.local/share/applications/

# Neovim - build from source
sudo apt install -y cmake make gcc g++
git clone https://github.com/neovim/neovim.git ~/works/neovim
cd ~/works/neovim
make CMAKE_BUILD_TYPE=RelWithDebInfo
sudo make install

# Zellij
cargo install zellij
sudo mv ~/.cargo/bin/zellij /usr/local/bin/
mkdir -p ~/.config/zellij/plugins
curl -L https://github.com/fresh2dev/zellij-autolock/releases/download/0.2.2/zellij-autolock.wasm \
    -o ~/.config/zellij/plugins/zellij-autolock.wasm

# Nerd fonts
mkdir -p ~/works/fonts ~/.fonts
wget -P ~/works/fonts https://github.com/ryanoasis/nerd-fonts/releases/download/v3.4.0/FiraCode.zip
wget -P ~/works/fonts https://github.com/ryanoasis/nerd-fonts/releases/download/v3.4.0/Mononoki.zip
wget -P ~/works/fonts https://github.com/ryanoasis/nerd-fonts/releases/download/v3.4.0/ComicShannsMono.zip
unzip "~/works/fonts/*.zip" -d ~/.fonts
# CJK / emoji / Korean fonts
sudo apt install -y fonts-noto fonts-noto-cjk fonts-noto-color-emoji fonts-nanum
fc-cache -fv

# bspwm + sxhkd
sudo apt install -y libxcb-xinerama0-dev libxcb-icccm4-dev libxcb-randr0-dev libxcb-util0-dev \
    libxcb-ewmh-dev libxcb-keysyms1-dev libxcb-shape0-dev
git clone https://github.com/baskerville/bspwm.git ~/works/bspwm
cd ~/works/bspwm && make && sudo make install
git clone https://github.com/baskerville/sxhkd.git ~/works/sxhkd
cd ~/works/sxhkd && make && sudo make install

# picom
sudo apt install -y libconfig-dev libdbus-1-dev libegl-dev libev-dev libgl-dev libepoxy-dev \
    libpcre2-dev libpixman-1-dev libx11-xcb-dev libxcb1-dev libxcb-composite0-dev \
    libxcb-damage0-dev libxcb-glx0-dev libxcb-image0-dev libxcb-present-dev libxcb-randr0-dev \
    libxcb-render0-dev libxcb-render-util0-dev libxcb-shape0-dev libxcb-util-dev \
    libxcb-xfixes0-dev meson ninja-build uthash-dev
git clone https://github.com/yshui/picom ~/works/picom
cd ~/works/picom
meson setup --buildtype=release . build
ninja -C build && sudo ninja -C build install

# polybar
sudo apt install -y cmake-data pkg-config python3-sphinx python3-packaging libuv1-dev libcairo2-dev \
    libxcb1-dev libxcb-util0-dev libxcb-randr0-dev libxcb-composite0-dev python3-xcbgen xcb-proto \
    libxcb-image0-dev libxcb-ewmh-dev libxcb-icccm4-dev libxcb-xkb-dev libxcb-xrm-dev \
    libxcb-cursor-dev libasound2-dev libpulse-dev libjsoncpp-dev libmpdclient-dev \
    libcurl4-openssl-dev libnl-genl-3-dev
git clone --recursive https://github.com/polybar/polybar ~/works/polybar
cd ~/works/polybar && mkdir build && cd build
cmake .. && make -j$(nproc) && sudo make install

# eww
sudo apt install -y libdbusmenu-gtk3-dev
git clone https://github.com/elkowar/eww ~/works/eww
cd ~/works/eww
cargo build --release --no-default-features --features x11
sudo cp ~/works/eww/target/release/eww /usr/local/bin/

# theme - arc-kde: KDE Plasma 테마, arc-theme: GTK 앱용
sudo apt install -y arc-theme arc-kde papirus-icon-theme python3-pip
pip3 install pywal

# network manager: Kubuntu에 plasma-nm 내장 (bspwm 세션에서 필요하면 별도 설치)
# sudo apt install -y network-manager-gnome

# i3lock-color - build from source
sudo apt install -y autoconf libpam0g-dev libcairo2-dev libfontconfig1-dev \
    libxcb-composite0-dev libev-dev libx11-xcb-dev libxcb-xkb-dev libxcb-xinerama0-dev \
    libxcb-randr0-dev libxcb-image0-dev libxcb-util0-dev libxcb-xrm-dev \
    libxkbcommon-dev libxkbcommon-x11-dev libjpeg-dev
git clone https://github.com/Raymo111/i3lock-color ~/works/i3lock-color
cd ~/works/i3lock-color && ./install-i3lock-color.sh

# ghostty (terminal) - build with zig
sudo apt install -y libgtk-4-dev libadwaita-1-dev
# install zig if not present
if ! command -v zig &>/dev/null; then
    ZIG_VER="0.14.0"
    wget -P ~/works "https://ziglang.org/download/${ZIG_VER}/zig-linux-x86_64-${ZIG_VER}.tar.xz"
    tar -xf ~/works/zig-linux-x86_64-${ZIG_VER}.tar.xz -C ~/works
    sudo ln -sf ~/works/zig-linux-x86_64-${ZIG_VER}/zig /usr/local/bin/zig
fi
git clone https://github.com/ghostty-org/ghostty ~/works/ghostty
cd ~/works/ghostty
zig build -p /usr/local -Doptimize=ReleaseFast

# ImageMagick
sudo apt install -y imagemagick

# file manager
sudo apt install -y thunar gvfs gvfs-backends tumbler

# yazi (terminal file manager)
sudo apt install -y ffmpegthumbnailer unar jq poppler-utils fd-find fzf zoxide chafa
cargo install yazi-fm yazi-cli
sudo mv ~/.cargo/bin/yazi* /usr/local/bin/

# ripgrep, eza
cargo install ripgrep eza
sudo mv ~/.cargo/bin/rg ~/.cargo/bin/eza /usr/local/bin/

# plocate
sudo apt install -y plocate
sudo systemctl enable --now plocate-updatedb.timer
# /home(btrfs 서브볼륨) 인덱싱 허용, USB(/run/media) 제외
sudo sed -i 's/PRUNE_BIND_MOUNTS = "yes"/PRUNE_BIND_MOUNTS = "no"/' /etc/updatedb.conf
sudo sed -i 's|PRUNEPATHS = "\(.*\)"|PRUNEPATHS = "\1 /run/media"|' /etc/updatedb.conf
sudo updatedb

# bspwm utils
sudo apt install -y xdotool xclip rofi xdo wmname xsettingsd volumeicon-alsa

# scpad / pythonpad
sudo apt install -y python3 python3-numpy python3-matplotlib

# Syncthing
curl -s https://syncthing.net/release-key.txt \
    | sudo gpg --dearmor -o /usr/share/keyrings/syncthing-archive-keyring.gpg
echo "deb [signed-by=/usr/share/keyrings/syncthing-archive-keyring.gpg] https://apt.syncthing.net/ syncthing stable" \
    | sudo tee /etc/apt/sources.list.d/syncthing.list
sudo apt update && sudo apt install -y syncthing
sudo systemctl enable --now syncthing@"$USER".service

# ExpressVPN
# Download .deb from https://www.expressvpn.com/vpn-software/vpn-linux then:
# sudo dpkg -i expressvpn_*.deb
# sudo systemctl enable --now expressvpn

# Emacs - build from source
sudo apt install -y autoconf libgtk-3-dev libgif-dev libgnutls28-dev libgccjit-14-dev \
    libxpm-dev libncurses-dev
git clone https://github.com/emacs-mirror/emacs.git ~/works/emacs
cd ~/works/emacs
git checkout emacs-30.2
./autogen.sh
./configure --with-native-compilation --with-json --with-modules
make -j$(nproc) && sudo make install

git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
~/.config/emacs/bin/doom install

# TeX Live + Korean
sudo apt install -y texlive texlive-lang-korean

# Jupyter
pip3 install jupyterlab

# zathura PDF viewer
sudo apt install -y zathura

# verilator
sudo apt install -y verilator

# sourcegit
SOURCEGIT_DEB=$(curl -s https://api.github.com/repos/sourcegit-scm/sourcegit/releases/latest \
    | grep -o 'https://.*linux.*\.deb' | head -1)
wget -P ~/works "$SOURCEGIT_DEB"
sudo dpkg -i ~/works/sourcegit*.deb

echo "Setup complete!"
