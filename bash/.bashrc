#
# ~/.bashrc
#
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

alias ls='ls -p --color'
alias ll='ls -l'
alias grep='grep --color'

export EDITOR='code -w'
export VISUAL='code -w'

export GPG_TTY=`tty`

export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64

parse_git_branch() {
    branch=$(git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/')
    if [[ -n $branch ]]; then
        printf '[%s]' $branch
    fi
}

PS1='${debian_chroot:+($debian_chroot)}\[\e[32m\]\u@\h\[\e[0m\]:\w\[\e[34m\]$(parse_git_branch)\[\e[0m\]\n\$ '
