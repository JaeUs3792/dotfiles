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

# ZSH
sudo apt install zsh figet
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ~/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting
git clone https://github.com/zsh-users/zsh-autosuggestions.git ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions
# Neovim
sudo apt install neovim curl
sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
nvim +PlugInstall +q +q

# Tmux
sudo apt install tmux
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
TMUX_PLUGIN_MANAGER_PATH=~/.config/tmux/plugins/tpm ~/.tmux/plugins/tpm/scripts/install_plugins.sh
TMUX_PLUGIN_MANAGER_PATH=~/.config/tmux/plugins/tpm ~/.tmux/plugins/tpm/bin/update_plugins all

# Nerd fonts
mkdir -p ~/Projects
wget https://github.com/ryanoasis/nerd-fonts/releases/download/v2.3.3/FiraCode.zip ~/Projects
wget https://github.com/ryanoasis/nerd-fonts/releases/download/v2.3.3/Mononoki.zip ~/Projects
unzip ~/Projects/*.zip -d ~/.fonts
fc-cache -fv
##################################################
## I3-WM
##################################################
# I3-gaps
sudo add-apt-repository ppa:regolith-linux/release
sudo apt update
sudo apt install i3-gaps

# picom
# prequisite
sudo apt install libxext-dev libxcb1-dev libxcb-damage0-dev libxcb-dpms0-dev libxcb-xfixes0-dev libxcb-shape0-dev libxcb-render-util0-dev libxcb-render0-dev libxcb-randr0-dev libxcb-composite0-dev libxcb-image0-dev libxcb-present-dev libxcb-glx0-dev libpixman-1-dev libdbus-1-dev libconfig-dev libgl-dev libegl-dev libpcre2-dev libevdev-dev uthash-dev libev-dev libx11-xcb-dev meson
git clone https://github.com/yshui/picom ~/Project/picom
cd ~/Project/picom
git submodule update --init --recursive
meson setup --buildtype=release . build
ninja -C build
ninja -C build install

# polybar
apt install build-essential git cmake cmake-data pkg-config python3-sphinx python3-packaging libuv1-dev libcairo2-dev libxcb1-dev libxcb-util0-dev libxcb-randr0-dev libxcb-composite0-dev python3-xcbgen xcb-proto libxcb-image0-dev libxcb-ewmh-dev libxcb-icccm4-dev
sudo apt install libxcb-xkb-dev libxcb-xrm-dev libxcb-cursor-dev libasound2-dev libpulse-dev i3-wm libjsoncpp-dev libmpdclient-dev libcurl4-openssl-dev libnl-genl-3-dev

# terminal
sudo snap install alacritty --classic

# Emacs
sudo snap install emacs --classic
sudo snap install ripgrep --classic
