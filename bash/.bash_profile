#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

export XKB_DEFAULT_MODEL=pc104
export XKB_DEFAULT_LAYOUT=us,us
export XKB_DEFAULT_VARIANT=,intl
export XKB_DEFAULT_OPTIONS=grp:ctrls_toggle,lv3:ralt_switch,caps:ctrl_modifier,esperanto:qwerty,nbsp:level2

export PATH=$HOME/bin:$PATH
export PATH="/home/ian/.guix-profile/bin:/home/ian/.guix-profile/sbin${PATH:+:}$PATH"

export GUIX_LOCPATH=$HOME/.guix-profile/lib/locale
