#!/usr/bin/env bash
# Launch Questa Sim inside the vivado-ubuntu distrobox container.
#
# Merges Questa-specific X resources first so the Tk GUI does not inherit
# pywal's global dark "*background" wildcard. See questasim.xresources.

set -euo pipefail

res="$(dirname "$(readlink -f "$0")")/questasim.xresources"

if [ -f "$res" ] && command -v xrdb >/dev/null 2>&1; then
    xrdb -merge "$res" || true
fi

exec /usr/bin/distrobox-enter -n vivado-ubuntu -- \
    env LM_LICENSE_FILE=/tools/questasim/license.dat \
    /tools/questasim/questasim/bin/vsim -gui "$@"
