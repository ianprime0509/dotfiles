# Honor per-interactive-shell startup file
if [ -f ~/.bashrc ]; then . ~/.bashrc; fi

export GOPATH=~/go
export PLAN9=/usr/local/plan9
export PATH=$HOME/bin:$PATH:$GOPATH/bin:$PLAN9/bin
