if status is-interactive
    # Commands to run in interactive sessions can go here
end

set fish_greeting

# Format man pages
set -x MANROFFOPT "-c"
set -x MANPAGER "sh -c 'col -bx | bat -l man -p'"

# Set settings for https://github.com/franciscolourenco/done
set -U __done_min_cmd_duration 10000
set -U __done_notification_urgency_level low

## Environment setup
# Apply .profile: use this to put fish compatible .profile stuff in
if test -f ~/.fish_profile
  source ~/.fish_profile
end

# Add ~/.local/bin to PATH
if test -d ~/.local/bin
    if not contains -- ~/.local/bin $PATH
        set -p PATH ~/.local/bin
    end
end

## Functions
# Functions needed for !! and !$ https://github.com/oh-my-fish/plugin-bang-bang
function __history_previous_command
  switch (commandline -t)
  case "!"
    commandline -t $history[1]; commandline -f repaint
  case "*"
    commandline -i !
  end
end

function __history_previous_command_arguments
  switch (commandline -t)
  case "!"
    commandline -t ""
    commandline -f history-token-search-backward
  case "*"
    commandline -i '$'
  end
end

if [ "$fish_key_bindings" = fish_vi_key_bindings ];
  bind -Minsert ! __history_previous_command
  bind -Minsert '$' __history_previous_command_arguments
else
  bind ! __history_previous_command
  bind '$' __history_previous_command_arguments
end

# Fish command history
function history
    builtin history --show-time='%F %T '
end

## Aliases
alias ls='eza -al --color=always --group-directories-first --icons' # preferred listing
alias la='eza -a --color=always --group-directories-first --icons'  # all files and dirs
alias ll='eza -l --color=always --group-directories-first --icons'  # long format
alias lt='eza -aT --color=always --group-directories-first --icons' # tree listing
alias l.="eza -a | grep -e '^\.'"                                     # show only dotfiles

alias big="expac -H M '%m\t%n' | sort -h | nl"              # Sort installed packages according to size in MB
alias gitpkg='pacman -Q | grep -i "\-git" | wc -l'          # List amount of -git packages
#pacman unlock
alias fixpacman="sudo rm /var/lib/pacman/db.lck"

# Get fastest mirrors
alias mirror="sudo cachyos-rate-mirrors"
# Cleanup orphaned packages
alias cleanup='sudo pacman -Rns (pacman -Qtdq)'
# Recent installed packages
alias rip="expac --timefmt='%Y-%m-%d %T' '%l\t%n %v' | sort | tail -200 | nl"

# emacs
alias emd="emacs --daemon"
alias emdk="emacsclient --eval '(kill-emacs)'"
alias em="emacsclient -c -a 'emacs'"
alias emt="emacsclient -c -a 'emacs -nw'"

#neovim to vi
alias vi=nvim
alias vimdiff="nvim -d"
alias cat=bat

#grub update
alias update-grub="sudo grub-mkconfig -o /boot/grub/grub.cfg"
#gpg for future uses
#verify signature for isos
#alias gpg-check="gpg2 --keyserver-options auto-key-retrieve --verify"
#alias fix-gpg-check="gpg2 --keyserver-options auto-key-retrieve --verify"
#receive the key of a developer
#alias gpg-retrieve="gpg2 --keyserver-options auto-key-retrieve --receive-keys"
#alias fix-gpg-retrieve="gpg2 --keyserver-options auto-key-retrieve --receive-keys"
#alias fix-key="[ -d ~/.gnupg ] || mkdir ~/.gnupg ; cp /etc/pacman.d/gnupg/gpg.conf ~/.gnupg/ ; echo 'done'"

#figlet -w 100 "$hostname"
export PYTHONSTARTUP=/home/jaeus/scripts/my_imports.py

starship init fish | source

atuin init fish | source
