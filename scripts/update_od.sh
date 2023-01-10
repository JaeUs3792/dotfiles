#!/bin/bash

# update my linux config
cd ~/.dotfiles
git pull

# update linux packages
paru
# update bitwarden
cd ~/server/bitwarden_docker
sudo docker-compose down
sudo docker-compose pull
sudo docker-compose up -d
# wetty
yarn global upgrade wetty
# doom emacs upgrade
~/.emacs.d/bin/doom upgrade


