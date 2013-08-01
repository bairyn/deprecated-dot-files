#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export LD_PRELOAD="/usr/lib/libtrash.so $LD_PRELOAD"
alias ls='ls --color=auto'
alias lt='ls'
alias l='ls'
#export PATH=~/bin:~/.cabal/bin:$PATH
PATH=~/bin:~/.cabal/bin:/usr/local/bin:/opt/java/bin:${PATH} && export PATH
PS1='[\u@\h \W]\$ '
export EDITOR=vim
export VISUAL=vim
