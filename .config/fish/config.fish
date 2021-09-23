if status is-interactive
    # Commands to run in interactive sessions can go here
	figlet -w 100 "JaeUs-Device"
end
set fish_color_normal brcyan
set fish_color_autosuggestion '#7d7d7d'
set fish_color_command brcyan
set fish_color_error '#ff6c6b'
set fish_color_param brcyan


set EDITOR "emacsclient -t -a ''"

alias vi=nvim
alias emacs="emacsclient -t"
alias paruunlock="sudo rm /var/lib/pacman/db.lck"
alias update-grub="sudo grub-mkconfig -o /boot/grub/grub.cfg"
alias ytv-best="youtube-dl -f bestvideo+bestaudio "

starship init fish | source
