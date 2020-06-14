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

# Other aliases.
alias cl="clear && tmux clear-history"  # Clear the screen.
alias -g L="| less"  # Allow e.g. "dmesg L" as shorthand for "dmesg | less".
alias -g V="| vipe"  # Allow e.g. "dmesg V" as shorthand for "dmesg | vipe".

# ssh-agent.
# Simple way to temporarily force overriding these values rather than reading
# from the environment.
if false; then
	export SSH_AGENT_SIMPLE="0"
	export SSH_AGENT_SOURCE_PATH="${HOME}/.ssh-agent.source"
	export SSH_AGENT_AUTO_START="1"
	export SSH_AGENT_AUTO_ADD="1"
	export SSH_AGENT_AUTO_CONNECT="1"
fi
export SSH_AGENT_SIMPLE="${SSH_AGENT_SIMPLE-"0"}"
export SSH_AGENT_SOURCE_PATH="${SSH_AGENT_SOURCE_PATH:-"${HOME}/.ssh-agent.source"}"
if test "${SSH_AGENT_SIMPLE:-0}" -ne "0" &>>/dev/null; then
	export SSH_AUTH_SOCK="$(echo "/tmp/ssh-"*"/agent."*)"
else
	export SSH_AGENT_AUTO_START="${SSH_AGENT_AUTO_START-"1"}"
	export SSH_AGENT_AUTO_ADD="${SSH_AGENT_AUTO_ADD-"1"}"
	export SSH_AGENT_AUTO_CONNECT="${SSH_AGENT_AUTO_CONNECT-"1"}"
	export SSH_AGENT_AUTO_CONNECT_AFTER_ADD="${SSH_AGENT_AUTO_CONNECT_AFTER_ADD-"1"}"

	ssh-agent-is-running() {
		set -o local_loops -o local_options -o local_patterns -o local_traps
		set -uE -o pipefail

		pgrep '^ssh-agent$' >>/dev/null && [[ -r "${SSH_AGENT_SOURCE_PATH}" ]] || return
	}

	ssh-agent-source() {
		set -o local_loops -o local_options -o local_patterns -o local_traps
		set -uE -o pipefail

		source -- "${SSH_AGENT_SOURCE_PATH}" || return
	}

	ssh-agent-new() {
		set -o local_loops -o local_options -o local_patterns -o local_traps
		set -uE -o pipefail

		ssh-agent > "${SSH_AGENT_SOURCE_PATH}" || return

		if test "${SSH_AGENT_AUTO_ADD:-0}" -ne "0" &>>/dev/null; then
			if test "${SSH_AGENT_AUTO_CONNECT_AFTER_ADD:-0}" -ne "0" &>>/dev/null; then
				ssh-agent-source || return
			else
				(
					ssh-agent-source || return
					ssh-add || return
				)
			fi
		fi
	}

	ssh-agent-restart() {
		set -o local_loops -o local_options -o local_patterns -o local_traps
		set -ueE -o pipefail

		if ssh-agent-is-running; then
			pkill '^ssh-agent$' || return
		fi

		ssh-agent-new || return
	}

	ssh-agent-ensure() {
		set -o local_loops -o local_options -o local_patterns -o local_traps
		set -uE -o pipefail

		if ! ssh-agent-is-running; then
			ssh-agent-new || return
		fi
	}

	ssh-agent-connect() {
		set -o local_loops -o local_options -o local_patterns -o local_traps
		set -uE -o pipefail

		ssh-agent-ensure || return
		ssh-agent-source || return
	}

	if test "${SSH_AGENT_AUTO_START:-0}" -ne "0" &>>/dev/null; then
		ssh-agent-ensure || printf '%s\n' "Failed to ensure ssh-agent is running."
	fi

	if test "${SSH_AGENT_AUTO_CONNECT:-0}" -ne "0" &>>/dev/null; then
		ssh-agent-connect || printf '%s\n' "Failed to connect to ssh-agent."
	fi
fi
