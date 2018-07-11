# Set up the prompt

autoload -Uz promptinit
promptinit
prompt adam1

setopt histignorealldups sharehistory

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e

# Use modern completion system
autoload -Uz compinit
compinit

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

# History options.

## Keep 1,000,000,000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=1000000000
SAVEHIST=1000000000
HISTFILE=~/.zsh_history
setopt hist_ignore_space  # Forget lines beginning with a space.
setopt no_hist_no_store  # Don't remove "history" commands from the history for some reason.
setopt hist_expire_dups_first  # TODO
setopt hist_fcntl_lock  # TODO
setopt no_hist_reduce_blanks  # Don't remove superfluous spaces.
setopt hist_verify  # If "!" is entered unexpanded, replace it with the command
                    # first on the prompt, before executing it immediately.

## Make parallel zsh sessions' histories more accessible to each other.
setopt no_share_history
setopt append_history
setopt inc_append_history
setopt inc_append_history_time

# File globbing: support ^, ~, and #.
setopt extended_glob

# Automatically pushd with cd.
setopt auto_pushd

# Allow directories to be changed into with an implied "cd".
setopt auto_cd

# Correction.
setopt no_correct      # Command.
setopt no_correct_all  # Command and arguments.  (It seems to be based on files
                       # in the current directory?  Probably not worth setting
                       # with "correct".)
setopt dvorak          # Use the dvorak keyboard layout as the basis for spell correction suggestions.

# Ignore comments on the interactive command line too.
setopt interactive_comments

# ls aliases.
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
