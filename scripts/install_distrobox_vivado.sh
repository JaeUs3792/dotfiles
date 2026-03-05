#!/bin/bash

CONTAINER_NAME="vivado-ubuntu"
IMAGE="ubuntu:22.04"

# Check distrobox
if ! command -v distrobox &>/dev/null; then
    echo "distrobox is not installed. Install it first."
    exit 1
fi

# Create container
echo "Creating distrobox container: $CONTAINER_NAME ($IMAGE)"
distrobox create --name "$CONTAINER_NAME" --image "$IMAGE" --yes

# Install dependencies inside container
echo "Installing Vivado 2024.2 dependencies..."
distrobox enter "$CONTAINER_NAME" -- sudo apt update
distrobox enter "$CONTAINER_NAME" -- sudo apt install -y \
    libncurses5 libncursesw5 libtinfo5 libx11-6 libxext6 libxrender1 \
    libxtst6 libxi6 libfreetype6 fontconfig libglib2.0-0 libsm6 \
    libxrandr2 libxcursor1 libxft2 locales gcc g++ make net-tools

# Generate locale
distrobox enter "$CONTAINER_NAME" -- sudo locale-gen en_US.UTF-8

echo ""
echo "=== Setup complete ==="
echo ""
echo "1. Enter the container:"
echo "   distrobox enter $CONTAINER_NAME"
echo ""
echo "2. Run the Vivado installer inside the container:"
echo "   /path/to/Xilinx_Unified_2024.2_xxx/xsetup"
echo ""
echo "3. Export Vivado to the host (run inside the container):"
echo "   distrobox-export --app vivado"
echo ""
