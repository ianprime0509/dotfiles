. $HOME/.profile
# No bell
xset b off
# This is easier on the eyes if no background is set
xsetroot -solid grey24
# The menu key is useless; the compose key is useful
setxkbmap -option compose:menu
dwmstatus &
# Execute dwm continuously so it can be restarted without logging off;
# sending SIGTERM or something will cause the exit code to be non-zero
while dwm >dwm.log 2>&1; do
done
