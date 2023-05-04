if status is-interactive
    # Commands to run in interactive sessions can go here
end

set fish_greeting

set VETHER_IP $(/bin/grep nameserver /etc/resolv.conf 2> /dev/null | /bin/tr -s ' ' | /bin/cut -d' ' -f2)
export DISPLAY=$VETHER_IP:10.0

# emacs
alias emupgrade="~/.emacs.d/bin/doom upgrade"
alias emsync="~/.emacs.d/bin/doom sync"
alias emd="emacs --daemon"
alias emdk="emacsclient --eval '(kill-emacs)'"
alias em="emacsclient -c -a 'emacs'"
alias emt="emacsclient -c -a 'emacs -nw'"
alias emtangle="emacs --batch -eval \"(require 'org)\" --eval '(org-babel-tangle-file \"~/.config/emacs/emacs.org\")'"
#neovim to vi
alias vi=nvim
alias cat=bat
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
alias startsv="ssh odroid startsv"
alias stopsv="ssh odroid stopsv"
alias gpulls="pushd ~/.config/emacs && git pull && pushd ~/org && git pull && popd && popd"

# Changing "ls" to "exa"
alias ls='exa -al --color=always --group-directories-first' # my preferred listing
alias la='exa -a --color=always --group-directories-first'  # all files and dirs
alias ll='exa -l --color=always --group-directories-first'  # long format
alias lt='exa -aT --color=always --group-directories-first' # tree listing
alias l.='exa -a | egrep "^\."'

#figlet -w 100 "$hostname"
export PYTHONSTARTUP=/home/jaeus/scripts/my_imports.py

starship init fish | source
atuin init fish | source
