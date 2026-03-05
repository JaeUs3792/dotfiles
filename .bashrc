# dotfiles master .bashrc (used by distrobox containers)

# distrobox: source container-specific bashrc
if [ -n "$CONTAINER_ID" ] && [ -f "$HOME/.dotfiles/distrobox/$CONTAINER_ID/.bashrc" ]; then
    . "$HOME/.dotfiles/distrobox/$CONTAINER_ID/.bashrc"
fi
