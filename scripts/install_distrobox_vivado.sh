#!/bin/bash

CONTAINER_NAME="vivado-ubuntu"
IMAGE="ubuntu:22.04"
MAC_ADDRESS="74:56:3c:cb:f3:05" # Vivado license MAC

# Check distrobox
if ! command -v distrobox &>/dev/null; then
    echo "distrobox is not installed. Install it first."
    exit 1
fi

# Create container
echo "Creating distrobox container: $CONTAINER_NAME ($IMAGE)"
DOTFILES_DISTROBOX="$HOME/.dotfiles/distrobox/$CONTAINER_NAME"

distrobox create --name "$CONTAINER_NAME" --image "$IMAGE" \
    --unshare-netns \
    --init-hooks "sudo chsh -s /bin/bash \$(whoami)" \
    --additional-flags "--network bridge --mac-address $MAC_ADDRESS -e _JAVA_AWT_WM_NONREPARENTING=1" --yes

# Install dependencies inside container
echo "Installing Vivado 2024.2 dependencies..."
distrobox enter "$CONTAINER_NAME" -- sudo apt update
distrobox enter "$CONTAINER_NAME" -- sudo apt install -y \
    libncurses5 libncursesw5 libtinfo5 libx11-6 libxext6 libxrender1 \
    libxtst6 libxi6 libfreetype6 fontconfig libglib2.0-0 libsm6 \
    libxrandr2 libxcursor1 libxft2 locales gcc g++ make net-tools \
    iproute2 gawk python3 xterm autoconf libtool texinfo zlib1g-dev \
    gcc-multilib build-essential libsdl1.2-dev libglib2.0-dev \
    screen pax gzip tar cpio rsync xvfb diffstat chrpath socat \
    lz4 zstd liblz4-tool bc u-boot-tools \
    lsb-release libncurses5-dev \
    libnss3 libatk1.0-0 libatk-bridge2.0-0 libdrm2 libgbm1 libasound2 \
    libgtk-3-0 libsecret-1-0

# Generate locale
distrobox enter "$CONTAINER_NAME" -- sudo locale-gen en_US.UTF-8

# Java AWT fix is applied via -e flag in distrobox create above

# Create install directories
distrobox enter "$CONTAINER_NAME" -- sudo mkdir -p /tools /opt/petalinux
distrobox enter "$CONTAINER_NAME" -- sudo chown "$(whoami)" /tools /opt/petalinux

# Symlink master .bashrc from dotfiles
ln -sf "$HOME/.dotfiles/.bashrc" "$HOME/.bashrc"

echo ""
echo "=== Setup complete ==="
echo ""
echo "1. Enter the container:"
echo "   distrobox enter $CONTAINER_NAME"
echo ""
echo "2. Run the Vivado installer inside the container:"
echo "   /path/to/Xilinx_Unified_2024.2_xxx/xsetup"
echo "   NOTE: Uncheck 'Create program group entries' during installation"
echo ""
echo "3. Run the PetaLinux installer inside the container:"
echo "   chmod +x petalinux-v2024.2-*-installer.run"
echo "   ./petalinux-v2024.2-*-installer.run -d /opt/petalinux"
echo ""
echo "4. Export Vivado to the host (run inside the container):"
echo "   distrobox-export --app vivado"
echo ""
