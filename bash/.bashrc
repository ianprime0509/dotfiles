# Bash initialization for interactive non-login shells and
# for remote shells (info "(bash) Bash Startup Files").

# Export 'SHELL' to child processes.  Programs such as 'screen'
# honor it and otherwise use /bin/sh.
export SHELL

if [[ $- != *i* ]]
then
    # We are being invoked from a non-interactive shell.  If this
    # is an SSH session (as in "ssh host command"), source
    # /etc/profile so we get PATH and other essential variables.
    [[ -n "$SSH_CLIENT" ]] && source /etc/profile

    # Don't do anything else.
    return
fi

# Source the system-wide file.
source /etc/bashrc

# Adjust the prompt depending on whether we're in 'guix environment'.
if [ -n "$GUIX_ENVIRONMENT" ]
then
    PS1='\u@\h \w [env]\$ '
else
    PS1='\u@\h \w\$ '
fi
alias ls='ls -p --color=auto'
alias ll='ls -l'
alias grep='grep --color=auto'

export EDITOR=edit
export VISUAL=edit

export GIT_EXEC_PATH="/home/ian/.guix-profile/libexec/git-core"
export TERMINFO_DIRS="/home/ian/.guix-profile/share/terminfo"
export ACLOCAL_PATH="/home/ian/.guix-profile/share/aclocal${ACLOCAL_PATH:+:}$ACLOCAL_PATH"
 export CPATH="/home/ian/.guix-profile/include${CPATH:+:}$CPATH"
 export LIBRARY_PATH="/home/ian/.guix-profile/lib${LIBRARY_PATH:+:}$LIBRARY_PATH"
export PYTHONPATH="/home/ian/.guix-profile/lib/python3.7/site-packages${PYTHONPATH:+:}$PYTHONPATH"
export GUIX_LOCPATH="$HOME/.guix-profile/lib/locale"
