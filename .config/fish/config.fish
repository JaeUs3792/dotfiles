if status is-interactive
    # Commands to run in interactive sessions can go here
end

set fish_greeting

oh-my-posh init fish --config ~/.poshthemes/craver.omp.json | source

# emacs
alias emupgrade="~/.emacs.d/bin/doom upgrade"
alias emd="emacs --daemon"
alias emdk="emacsclient --eval '(kill-emacs)'"
alias em="emacsclient -c -a 'emacs'"
alias emt="emacsclient -c -a 'emacs -nw'"
alias emtangle="emacs --batch -eval \"(require 'org)\" --eval '(org-babel-tangle-file \"~/.config/emacs/emacs.org\")'"
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

#!/bin/sh
set hostname $(cat /etc/hostname)
figlet -w 100 "$hostname"
export PYTHONSTARTUP=/home/jaeus/scripts/my_imports.py
