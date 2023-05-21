#!/bin/sh
###############################################################################
#                               Initial Settings                               #
###############################################################################
# Locale
echo ko_KR.UTF-8 UTF-8 >> /etc/locale.gen
locale-gen
echo LANG=ko_KR.UTF-8 > /etc/locale.conf
echo [network] > /etc/wsl.conf
echo generateHosts = false >> /etc/wsl.conf
# User addition
EDITOR=vim visudo	# uncomment %wheel ALL=(ALL) ALL
useradd -m -g users -G wheel -s /bin/bash jaeus
passwd jaeus
# Change User
su jaeus
sudo pacman-key --init
sudo pacman-key --populate archlinux
# Configuration Copy
sudo pacman -S git stow --noconfirm
git clone https://github.com/JaeYoo-Im/dotfiles ~/.dotfiles
cd ~/.dotfiles
stow .
# AUR Manager
git clone https://aur.archlinux.org/paru-bin.git
cd paru-bin
makepkg -si
###############################################################################
#                                    Shell                                     #
###############################################################################
paru -S fish starship atuin --noconfirm

# Nushell
#paru -S nushell starship atuin --noconfirm
#mkdir ~/.local/share/atuin/
#atuin init nu | save ~/.local/share/atuin/init.nu
#chsh -s /bin/nu   # change shell

# TMUX (session maanger)
paru -S tmux --noconfirm
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
TMUX_PLUGIN_MANAGER_PATH=~/.config/tmux/plugins/tpm ~/.tmux/plugins/tpm/scripts/install_plugins.sh
TMUX_PLUGIN_MANAGER_PATH=~/.config/tmux/plugins/tpm ~/.tmux/plugins/tpm/bin/update_plugins all
##################################################
# tldr
##################################################
paru -S tldr --noconfirm

##################################################
# Wallpaper
##################################################
paru -S python-pywal xorg-xrdb --noconfirm

git clone https://git.jaeus.net/walls


##################################################
# Fonts
##################################################
paru -S ttf-firacode-nerd ttf-momonoki-nerd ttf-times-new-roman ttf-nanum noto-fonts-emoji ttf-symbola noto-font-cjk --noconfirm

##################################################
# EMACS Related
##################################################
# base
paru -S ripgrep emacs texlive-most texlive-lang --noconfirm

# Python
paru -S python python-pip jupyter openssh inetutils --noconfirm
pip install matplotlib numpy pandas tabulate

# verilator for syntax checker
paru -S verilator --noconfirm
paru -S uctags-git --noconfirm

# nov
paru -S zip unzip --noconfirm

##################################################
# EXWM
##################################################
paru -S picom --noconfirm
paru -S xorg-xrandr feh cronie --noconfirm

##################################################
# Extra
##################################################
paru -S bat

