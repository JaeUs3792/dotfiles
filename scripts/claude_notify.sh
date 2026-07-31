#!/usr/bin/env bash
# Claude Code notification hook.
#
# Two signals, because one alone isn't enough here: a dunst popup for the
# desktop you're looking at, and an X urgency hint on the Emacs frame that owns
# the session so its bspwm desktop lights up in polybar when you're elsewhere.
#
# Wired from ~/.claude/settings.json on the Notification and Stop events.
set -u

payload=$(cat)
event=$(jq -r '.hook_event_name // "Notification"' <<<"$payload")
cwd=$(jq -r '.cwd // ""' <<<"$payload")
message=$(jq -r '.message // ""' <<<"$payload")
project=$(basename "${cwd:-$PWD}")

case "$event" in
    # Claude is blocked on the user: stays on screen until dismissed.
    Notification) urgency=critical; body="${message:-Waiting for your input}" ;;
    Stop)         urgency=low;      body="${message:-Finished}" ;;
    *)            urgency=normal;   body="${message:-$event}" ;;
esac

notify-send -a "Claude Code" -u "$urgency" "Claude Code · $project" "$body" 2>/dev/null

# Frame lookup table is populated by the advice in .config/emacs/lisp/init-ai.el.
# Empty when the session isn't running under claude-code-ide, which is fine --
# the popup above already fired.
wid=$(emacsclient --eval \
    "(let ((f (gethash \"${cwd%/}/\" ju/claude-ide--session-frames)))
       (if (frame-live-p f) (frame-parameter f 'outer-window-id) \"\"))" \
    2>/dev/null | tr -d '"')

# bspwm only clears the urgent flag when the node gets focus, so this reads as
# an unread marker rather than a blink.
[ -n "$wid" ] && xdotool set_window --urgency 1 "$wid" 2>/dev/null

exit 0
