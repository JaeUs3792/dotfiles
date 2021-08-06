#!/bin/sh
paru -S hyperv --noconfirm

sudo systemctl enable hv_fcopy_daemon.service
sudo systemctl enable hv_kvp_daemon.service
sudo systemctl enable hv_vss_daemon.service

git clone https://github.com/Microsoft/linux-vm-tools
cd linux-vm-tools/arch
./makepkg.sh
sudo ./install-config.sh

echo "type ""Set-VM -VMName Arch-HV -EnhancedSessionTransportType HvSocket"" in powershell"

echo xmonad >> ~/.xinitrc
