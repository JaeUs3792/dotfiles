#!/bin/bash
# Battery module for polybar — outputs nothing if no battery found

BAT_PATH=$(ls -d /sys/class/power_supply/BAT* 2>/dev/null | head -1)
[[ -z "$BAT_PATH" ]] && exit 0

capacity=$(cat "$BAT_PATH/capacity" 2>/dev/null) || exit 0
status=$(cat "$BAT_PATH/status" 2>/dev/null)

if [[ "$status" == "Charging" ]]; then
    icon=""
elif [[ "$capacity" -ge 90 ]]; then
    icon=""
elif [[ "$capacity" -ge 70 ]]; then
    icon=""
elif [[ "$capacity" -ge 50 ]]; then
    icon=""
elif [[ "$capacity" -ge 25 ]]; then
    icon=""
else
    icon=""
fi

echo "$icon ${capacity}%"
