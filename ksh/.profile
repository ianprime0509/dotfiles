# $OpenBSD: dot.profile,v 1.5 2018/02/02 02:29:54 yasuoka Exp $
#
# sh/ksh initialization

export PLAN9=/usr/local/plan9
PATH=$HOME/bin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/X11R6/bin:/usr/local/bin:/usr/local/sbin:/usr/games:$PLAN9/bin
export PATH HOME TERM
export ENV=$HOME/.kshrc
[ -f "$ENV" ] && . "$ENV"
