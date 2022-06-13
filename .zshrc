export ZSH="/home/jaeus/.oh-my-zsh"
ZSH_THEME="random"
#plugins=(git)
source $ZSH/oh-my-zsh.sh
prompt_context(){}
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

##WSL2
#VETHER_IP=$(/bin/grep nameserver /etc/resolv.conf 2> /dev/null | /bin/tr -s ' ' | /bin/cut -d' ' -f2)
#export DISPLAY=$VETHER_IP:10.0

# emacs
alias emupgrade="~/.emacs.d/bin/doom upgrade"
alias emd="emacs --daemon"
alias emdk="emacsclient --eval '(kill-emacs)'"
alias em="emacsclient -c -a 'emacs'"
alias emt="emacsclient -c -a 'emacs -nw'"
#neovim to vi
alias vi=nvim
#pacman unlock
alias paruunlock="sudo rm /var/lib/pacman/db.lck"
#grub update
alias update-grub="sudo grub-mkconfig -o /boot/grub/grub.cfg"
#youtube-dl
alias ytv-best="youtube-dl -f bestvideo+bestaudio "
#gpg for future uses
#verify signature for isos
#alias gpg-check="gpg2 --keyserver-options auto-key-retrieve --verify"
#alias fix-gpg-check="gpg2 --keyserver-options auto-key-retrieve --verify"
#receive the key of a developer
#alias gpg-retrieve="gpg2 --keyserver-options auto-key-retrieve --receive-keys"
#alias fix-gpg-retrieve="gpg2 --keyserver-options auto-key-retrieve --receive-keys"
#alias fix-key="[ -d ~/.gnupg ] || mkdir ~/.gnupg ; cp /etc/pacman.d/gnupg/gpg.conf ~/.gnupg/ ; echo 'done'"

hostname=$(cat /etc/hostname)
figlet -w 100 "$hostname"

#cat ~/.cache/wal/sequences &

# Fire it up for WSL2
#VETHER_IP=$(/bin/grep nameserver /etc/resolv.conf 2> /dev/null | /bin/tr -s ' ' | /bin/cut -d' ' -f2)
#export DISPLAY=$VETHER_IP:10.0
#export DISPLAY=192.168.144.1:10.0
#export EDITOR=nvim

export PYTHONSTARTUP=~/scripts/my_imports.py
