# vivado-ubuntu container bashrc

# Source Vivado settings
if [ -f /tools/Xilinx/Vivado/2024.2/settings64.sh ]; then
    source /tools/Xilinx/Vivado/2024.2/settings64.sh
fi

# Source PetaLinux settings
if [ -f /opt/petalinux/settings.sh ]; then
    source /opt/petalinux/settings.sh
fi
