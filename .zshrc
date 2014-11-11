# Lines configured by zsh-newuser-install
#setopt all_export
HISTFILE=~/.zhistfile
SAVEHISTFILE=~/.zhistfile
HISTSIZE=1000000
SAVEHIST=1000000
#HISTSIZE=1024
#SIZEHIST=9999999
setopt inc_append_history
setopt append_history
setopt always_last_prompt
setopt auto_cd
setopt auto_list
setopt auto_menu
setopt auto_name_dirs
setopt auto_param_keys
setopt auto_param_slash
setopt auto_pushd
setopt auto_remove_slash
setopt auto_resume
setopt correct
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_no_store
setopt no_hup
#setopt no_beep
setopt beep
setopt APPEND_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_FCNTL_LOCK
setopt rc_quotes
bindkey -v
bindkey -M viins '^?' backward-delete-char
bindkey -M viins '^H' backward-delete-char

bindkey -M vicmd 'd' vi-backward-char
bindkey -M vicmd 'h' vi-down-line-or-history
bindkey -M vicmd 't' vi-up-line-or-history
bindkey -M vicmd 'n' vi-forward-char
bindkey -M vicmd 'j' vi-delete
bindkey -M vicmd 'J' vi-kill-eol
bindkey -M vicmd 'K' vi-join
bindkey -M vicmd 'l' vi-repeat-search
# TODO: figure out how to generate the below "^@" control character in vim (is
# it with ctrl-o or something?)?  Then document in a comment how to generate
# this, noting that this character can be restored by pressing typing this
# sequence in vim, and then change the actual binding to ASCII "^@".  Why?
# Because some tools, eg grep, diff, and git diff, incorrectly identify this
# configuration file, .zshrc, as a binary file rather than a text file.
# But I don't want to change the " " below to "^@" until I document how I even
# wrote that character *in vim* in the first place.  Also note there's another
# occurrence of this non-printable character in a comment shortly below, and
# *this* paragraph also introduces one that can go away when the actual
# documentation is written.
bindkey -M viins ' ' vi-cmd-mode
#bindkey -M vicmd 'h' vi-backward-char
#bindkey -M vicmd 't' vi-down-line-or-history
#bindkey -M vicmd 'n' vi-up-line-or-history
#bindkey -M vicmd 's' vi-forward-char
#bindkey -M vicmd 'd' vi-delete
#bindkey -M vicmd 'D' vi-kill-eol
#bindkey -M vicmd 'K' vi-join
#bindkey -M vicmd 'l' vi-repeat-search
#bindkey -M viins ' ' vi-cmd-mode
#eval `ssh-agent -t 3600`  # One hour
export EDITOR=vim
export VISUAL=vim
setopt extendedglob
#source ~/.zsh_prompt
# End of lines configured by zsh-newuser-install
#export PROMPT="%i -%t - %n@%m:%c%# "  # shorter DIR
export PROMPT="%i - %T - %n@%m:%~%# "
#PATH=~/.cabal/bin:/usr/local/bin:/opt/java/bin:${PATH} && export PATH
#if [ -d ~/bin ]; then
#PATH=~/bin:${PATH} && export PATH
#fi
#alias lt="lt --color"
chpwd()
{
	print -Pn ']2;%~'
}
MAIL=/var/mail/bairyn && export MAIL
MAILDIR=/var/mail/bairyn && export MAIL

alias process='if [[ -z "$PROG" || -z "$FILES" ]]; then echo -ne "aborting: PROG or FILES is empty or not set\n" >& 2; false; else for FILE in $FILES; do cp -i $FILE .${FILE}.pbak; TMP=$(mktemp); ${=PROG} < $FILE > $TMP || echo -ne "Failed to process ${FILE}\n\n" && mv $TMP $FILE; done; unset PROG FILES; fi' # if user denies backing up file, it will still be overwritten unless the user cancels with ^C
alias processq='if [[ -z "$PROG" || -z "$FILES" ]]; then echo -ne "aborting: PROG or FILES is empty or not set\n" >& 2; false; else for FILE in $FILES; do cp $FILE .${FILE}.pbak; TMP=$(mktemp); ${=PROG} < $FILE > $TMP || echo -ne "Failed to process ${FILE}\n\n" && mv $TMP $FILE; done; unset PROG FILES; fi' # ALWAYS overwrite bak file if it exists
alias bell='echo -ne "\a"'
alias screenshot="scrot '%Y-%m-%d--%H:%M%S_\$wx\$h.png' -e 'mv \$f ~/screenshots/'"
export SDL_AUDIODRIVER="dsp"

#export LD_PRELOAD="${LD_PRELOAD}, /usr/lib/libtrash.so"
#export LD_PRELOAD="/usr/lib/libtrash.so"
#export LD_PRELOAD="/usr/lib/libtrash.so $LD_PRELOAD"

### ## ssh-agent BEGIN
### test=`/bin/ps -ef | /bin/grep ssh-agent | /bin/grep -v grep  | /usr/bin/awk '{print $2}' | xargs`
### 
### if [ "$test" = "" ]; then
   ### # there is no agent running
   ### if [ -e "$HOME/agent.sh" ]; then
      ### # remove the old file
      ### /bin/rm -f $HOME/agent.sh
   ### fi;
   ### # start a new agent
### #/usr/bin/ssh-agent | /usr/bin/grep -v echo >&$HOME/agent.sh
   ### /usr/bin/ssh-agent | /bin/grep -v echo >&$HOME/agent.sh
### fi;
### 
### test -e $HOME/agent.sh && source $HOME/agent.sh
### 
### alias kagent="kill -9 $SSH_AGENT_PID"
### ## ssh-agent END

# for tmux: export 256color
[ -n "$TMUX" ] && export TERM=screen-256color

#alias mplayercd='mplayer -cdrom-device /dev/sg1 -cache 15000 -cache-min 80 cdda://'
#alias mplayercdnoprefix='mplayer -cdrom-device /dev/sg1'

export LD_PRELOAD="/usr/lib/libtrash.so $LD_PRELOAD"
alias ls='ls --color=auto'
alias lt='ls'
alias l='ls'
#export PATH=~/bin:~/.cabal/bin:$PATH
#PATH=~/bin:~/.cabal/bin:/usr/local/bin:/opt/java/bin:${PATH} && export PATH
#PATH=~/bin:~/.cabal/bin:~/.gem/ruby/1.9.1/bin:/usr/local/bin:/opt/java/bin:${PATH} && export PATH
#PATH=~/bin:~/.cabal/bin:~/.gem/ruby/2.0.0/bin:~/.gem/ruby/1.9.3/bin:~/.gem/ruby/1.9.1/bin:/usr/local/bin:/opt/java/bin:${PATH} && export PATH
PATH=~/bin:.cabal-sandbox/bin:~/.cabal/bin:~/.gem/ruby/1.9.1/bin:/usr/local/bin:/opt/java/bin:${PATH} && export PATH

#eval `ssh-agent`
#export TERM=xterm
export TERM=xterm-256color

#wmname LG3D

#alias agda-wl="agda -i ./ -i ~/agda-lib"
#alias agda-wl="cat ~/agda2/opts | xargs agda -i ./ -i ./src -i ~/agda-lib"
#alias agda-wl="cat ~/agda2/opts | xargs ~/.cabal/bin/agda -i ./ -i ./src"

alias cls="echo -ne '\x1Bc'"
alias cl=cls
alias racket-wl="racket -il readline"
alias rsync="/usr/bin/rsync -xravzP"

# XCompose for some apps.
export GTK_IM_MODULE=xim
export QT_IM_MODULE=xim
export LANG=en_GB.UTF-8
export LANGUAGE=en_GB.UTF-8

#alias git0="unset GIT_WORK_TREE GIT_DIR"
#alias git1="export GIT_WORK_TREE=. GIT_DIR=.git_"
#alias git2="export GIT_WORK_TREE=. GIT_DIR=.git_"

alias v="vim -R"

alias mv="mv -i"
alias cp="cp -i"


alias fix-gtk="exec sudo -su bairyn"
alias fix-java="wmname LG3D"
#export JAVA_HOME=/usr/lib/jvm/default

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

source ~/.rvm/scripts/rvm
#rvm rvmrc trust ~/git/gless
#rvm rvmrc trust ~/git/gless-bairyn

source ~/.bashrc_ssh

alias pacman32="pacman --root /opt/arch32 --cachedir /opt/arch32/var/cache/pacman/pkg --config /opt/arch32/pacman.conf"
alias run32="schroot -c arch32 -p --"
alias schroot32="schroot -c arch32 -p --"

#export LANG=ru_RU.utf8
#export LANGUAGE=ru_RU.utf8

alias mplayer-wl="mplayer -af scaletempo -fixed-vo -loop 0 -speed 1"

export LD_LIBRARY_PATH=~/lib:$LD_LIBRARY_PATH

export SDL_AUDIODRIVER=pulse

#alias snc="sudo netcfg"
alias snc="sudo netctl"
export LANGUAGE=en LANG=en_GB.utf8

alias sos='while true; do sleep 2; for I in `seq 1 3`; do beep -f 400 -l 500; sleep 0.5; done; done'

alias updick='/usr/bin/uptime | perl -ne "/(\d+) d/;print 8,q(=)x\$1,\"D\n\""'

#wmname LG3D

#cowsay -f "$(ls /usr/share/cows/ | sort -R | head -1)" "$(fortune -s)"

alias lynx="lynx -accept_all_cookies"

alias beep-wl="beep -f 400 -l 100"

source ~/.priv/zshrc
