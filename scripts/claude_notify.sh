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

# Division of labour between the two signals: the popup is the *transient* cue
# for when you're at the screen, the urgency hint below is the *persistent* one
# for when you're not. So no popup needs to outlive its moment except the one
# that Claude is actually blocked on -- and letting the others expire keeps them
# from piling up against dunst's notification_limit, past which new popups are
# silently queued instead of shown.
popup=yes
timeout=30000

case "$event" in
    Notification)
        body="${message:-Waiting for your input}"
        case "$message" in
            # Blocked on the user: no timeout, this one has to be acted on.
            *permission*)
                urgency=critical; timeout=0 ;;
            # Fires only after Claude Code's internal 60s idle threshold, by
            # which point the Stop popup has long since said the same thing.
            # Suppressed as redundant; the urgency hint still gets set.
            *"waiting for your input"*)
                popup=no; urgency=low ;;
            *)
                urgency=critical; timeout=0 ;;
        esac ;;
    # Fires the instant the turn ends, so this -- not the idle Notification
    # above -- is the timely "your turn" signal, and is pitched to be seen.
    Stop) urgency=normal; body="${message:-Finished}" ;;
    *)    urgency=normal; body="${message:-$event}" ;;
esac

[ "$popup" = yes ] &&
    notify-send -a "Claude Code" -u "$urgency" -t "$timeout" \
        "Claude Code · $project" "$body" 2>/dev/null

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
