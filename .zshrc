export ZSH="/home/jaeus/.oh-my-zsh"
#ZSH_THEME="agnoster"
#plugins=(git)
source $ZSH/oh-my-zsh.sh
prompt_context(){}
source $ZSH_CUSTOM/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source $ZSH_CUSTOM/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

# emacs
alias emd="emacs --daemon"
alias emdk="emacsclient --eval '(kill-emacs)'"
alias em="emacsclient -c -a 'emacs'"
alias emt="emacsclient -c -a 'emacs -nw'"
alias emtangle="emacs --batch -eval \"(require 'org)\" --eval '(org-babel-tangle-file \"~/.config/emacs/emacs.org\")'"
# neovim
alias vi=nvim
alias vimdiff="nvim -d"
export EDITOR=nvim
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

# server on/off
alias startsv="ssh odroid startsv"
alias stopsv="ssh odroid stopsv"

alias ls="eza -a --icons"
alias ll="eza -al --icons"
alias lt="eza -a --tree --level=1 --icons"

# WSL2
if [[ $JUPCID == "WSL" ]];then
	VETHER_IP=$(/bin/grep nameserver /etc/resolv.conf 2> /dev/null | /bin/tr -s ' ' | /bin/cut -d' ' -f2)
	export DISPLAY=$VETHER_IP:10.0
fi

# python env
export PYTHONSTARTUP=~/scripts/my_imports.py

eval "$(starship init zsh)"
