PS1='\u@\h \w\$ '
export EDITOR=E
export VISUAL=E
export MANPATH=:/usr/local/share/man:/usr/local/plan9/man
alias inbox="mlist $HOME/mail/personal/INBOX | msort -d | mseq -S | mthread | mscan"
