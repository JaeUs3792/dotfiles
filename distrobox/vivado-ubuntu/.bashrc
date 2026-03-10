# vivado-ubuntu container bashrc

# Source Vivado settings
if [ -f /tools/Xilinx/Vivado/2024.2/settings64.sh ]; then
    source /tools/Xilinx/Vivado/2024.2/settings64.sh
fi

# Source PetaLinux settings
if [ -f /tools/petalinux/2024.2/settings.sh ]; then
    source /tools/petalinux/2024.2/settings.sh
fi

export LM_LICENSE_FILE=/tools/questasim/license.dat
export PATH=$PATH:/tools/questasim/questasim/bin
