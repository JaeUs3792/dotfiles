#!/bin/sh
sudo apt update && sudo apt upgrade
sudo apt install git
sudo apt install stow

# git config
git config --global user.name "JaeYoo-Im"
git config --global user.email "cpu3792@gmail.com"
git config --global user.autocrlf input

cd ~/.dotfiles
stow .

# Fish
sudo apt install fish curl
curl -sS https://starship.rs/install.sh | sh

# Neovim
# build from source
sudo apt install cmake make
mkdir -p ~/works
git clone https://github.com/neovim/neovim.git ~/works/neovim
cd ~/works/neovim
make CMAKE_BUILD_TYPE=RelWithDebInfo
sudo make install

# Tmux
sudo apt install tmux
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
TMUX_PLUGIN_MANAGER_PATH=~/.config/tmux/plugins/tpm ~/.tmux/plugins/tpm/scripts/install_plugins.sh
TMUX_PLUGIN_MANAGER_PATH=~/.config/tmux/plugins/tpm ~/.tmux/plugins/tpm/bin/update_plugins all

# Nerd fonts
wget https://github.com/ryanoasis/nerd-fonts/releases/download/v3.4.0/FiraCode.zip ~/works
wget https://github.com/ryanoasis/nerd-fonts/releases/download/v3.4.0/Mononoki.zip ~/works
wget https://github.com/ryanoasis/nerd-fonts/releases/download/v3.4.0/ComicShannsMono.zip ~/works

unzip ~/works/*.zip -d ~/.fonts
fc-cache -fv

# bspwm
#
sudo apt-get install libxcb-xinerama0-dev libxcb-icccm4-dev libxcb-randr0-dev libxcb-util0-dev libxcb-ewmh-dev libxcb-keysyms1-dev libxcb-shape0-dev
git clone https://github.com/baskerville/bspwm.git ~/works/bspwm
cd ~/works/bspwm
make
sudo make install

# picom
# prequisite
sudo apt install libconfig-dev libdbus-1-dev libegl-dev libev-dev libgl-dev libepoxy-dev libpcre2-dev libpixman-1-dev libx11-xcb-dev libxcb1-dev libxcb-composite0-dev libxcb-damage0-dev libxcb-glx0-dev libxcb-image0-dev libxcb-present-dev libxcb-randr0-dev libxcb-render0-dev libxcb-render-util0-dev libxcb-shape0-dev libxcb-util-dev libxcb-xfixes0-dev meson ninja-build uthash-dev
git clone https://github.com/yshui/picom ~/works/picom
cd ~/works/picom
meson setup --buildtype=release . build
ninja -C build
ninja -C build install

# polybar
sudo apt install build-essential git cmake cmake-data pkg-config python3-sphinx python3-packaging libuv1-dev libcairo2-dev libxcb1-dev libxcb-util0-dev libxcb-randr0-dev libxcb-composite0-dev python3-xcbgen xcb-proto libxcb-image0-dev libxcb-ewmh-dev libxcb-icccm4-dev
sudo apt install libxcb-xkb-dev libxcb-xrm-dev libxcb-cursor-dev libasound2-dev libpulse-dev i3-wm libjsoncpp-dev libmpdclient-dev libcurl4-openssl-dev libnl-genl-3-dev
git clone --recursive https://github.com/polybar/polybar ~/works/polybar
cd ~/works/polybar
mkdir build
cd build
cmake ..
make -j4
sudo make install

# eww
sudo apt install libdbusmenu-gtk3-dev
git clone https://github.com/elkowar/eww ~/works/eww
cd ~/works/eww
cargo build --release --no-default-features --features x11

# imagemagick
git clone https://github.com/ImageMagick/ImageMagick.git ~/works/ImageMagick
cd ~/works/ImageMagick
./configure --with-modules --enable-hdri --with-quantum-depth=16
make -j$(nproc)
sudo make install
sudo ldconfig

# terminal
sudo apt install rustup
rustup toolchain install stable

sudo apt install cmake g++ pkg-config libfontconfig1-dev libxcb-xfixes0-dev libxkbcommon-dev python3
cargo install alacritty

# eza
cargo install eza
cargo install ripgrep

sudo mv ~/.cargo/bin/* /usr/bin

# scpad / pythonpad
sudo apt install xdo xdotool python3 python3-numpy python3-matplotlib

# Emacs
sudo apt install autoconf libgtk-3-dev libgif-dev libgnutls28-dev libgccjit-14-dev libxpm-dev libncurses-dev
git clone git clone https://github.com/emacs-mirror/emacs.git ~/works/emacs
cd ~/works/emacs
git checkout emacs-30.2
./autogen.sh
./configure
make -j8

git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
.config/emacs/bin/doom install
