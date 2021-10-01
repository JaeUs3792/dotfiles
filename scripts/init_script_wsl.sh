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

# AUR Manager
sudo pacman -S base-devel rust --noconfirm
cd ~
git clone https://aur.archlinux.org/paru.git
cd paru
makepkg -si

# Configuration Copy
paru -S git stow --noconfirm
git clone https://github.com/JaeYoo-Im/dotfiles ~/.dotfiles
cd ~/.dotfiles
stow .

# ZSH
paru -S zsh oh-my-zsh-git zsh-syntax-highlighting zsh-autosuggestions figlet --noconfirm
/usr/share/oh-my-zsh/tools/install.sh
mv ~/.zshrc.* ~/.zshrc # overwrite configuration

# TMUX
paru -S tmux --noconfirm
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
TMUX_PLUGIN_MANAGER_PATH=~/.config/tmux/plugins/tpm ~/.tmux/plugins/tpm/scripts/install_plugins.sh
TMUX_PLUGIN_MANAGER_PATH=~/.config/tmux/plugins/tpm ~/.tmux/plugins/tpm/bin/update_plugins all

##################################################
# EMACS Related
##################################################
# base
paru -S ripgrep emacs ttf-fira-code ttf-d2coding auctex texlive-most texlive-lang --noconfirm

# Python
paru -S python python-pip jupyter openssh inetutils --noconfirm
pip install matplotlib numpy pandas tabulate

# verilog lsp
paru -S verilator --noconfirm
npm install -g @imc-trading/svlangserver
