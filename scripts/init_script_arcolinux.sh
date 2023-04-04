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
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ~/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting
git clone https://github.com/zsh-users/zsh-autosuggestions.git ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions
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
git config --global user.autocrlf input

# tmux
paru -S tmux --noconfirm
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
TMUX_PLUGIN_MANAGER_PATH=~/.config/tmux/plugins/tpm ~/.tmux/plugins/tpm/scripts/install_plugins.sh
TMUX_PLUGIN_MANAGER_PATH=~/.config/tmux/plugins/tpm ~/.tmux/plugins/tpm/bin/update_plugins all

# font
paru -S ttf-firacode-nerd ttf-nanum ttf-mononoki-nerd --noconfirm

# hangul
paru -S fcitx-hangul fcitx-configtool --noconfirm

# enpass
#paru -S enpass --noconfirm
#paru -S bitwarden --noconfirm
#paru -S green-tunnel --noconfirm

#emacs
paru -S ripgrep emacs --noconfirm
paru -S texlive-most texlive-lang --noconfirm
paru -S jupyterlab --noconfirm
paru -S zathura zathura-cb zathura-pdf-mupdf

#syncthing
paru -S syncthing --noconfirm
mkdir SyncThing
sudo systemctl enable syncthing@jaeus.service

# etc
paru -S figlet --noconfirm
