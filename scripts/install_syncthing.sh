#!/bin/bash 

pacman -S syncthing --noconfirm
systemctl enable --now syncthing@jaeus.service
