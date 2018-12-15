# Honor per-interactive-shell startup file
if [ -f ~/.bashrc ]; then . ~/.bashrc; fi

export GOPATH=~/go
export PATH=$HOME/bin:$PATH:$GOPATH/bin
