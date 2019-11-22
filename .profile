# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022
umask 002

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# Add directories to PATH.
hi() {
	local -a mypaths=(
		"${HOME}/.cargo/bin"  # cargo for rust
		"${HOME}/.cabal/bin"  # cabal for Haskell
		"${HOME}/bin"         # user bin
		"${HOME}/.local/bin"  # user bin
		"/opt/ghc/bin"        # https://launchpad.net/~hvr/+archive/ubuntu/ghc
	)
	((${#mypaths[@]} <= 0)) || \
	for mypath in "${mypaths[@]}"; do
		if ! [ -d "${mypath}" ]; then
			:
			1>&2 echo "Warning: not adding to PATH non-existent directory '${mypath}'"
		else
			if grep -Fq ":${mypath}:" <<< "${PATH}"; then
				:
				1>&2 echo "Warning: not adding duplicate entry to PATH for directory '${mypath}'"
			else
				PATH="${mypath}:${PATH}"
			fi
		fi
	done
} && hi

# Gnome configuration.
# Set capslock as a control (c.f. https://askubuntu.com/a/633539).
# setxkbmap dvorak -option ctrl:nocaps && xset r rate 150 64
gsettings set org.gnome.desktop.input-sources sources "[('xkb', 'us+dvorak'), ('xkb', 'us')]"
gsettings set org.gnome.desktop.input-sources xkb-options "['ctrl:nocaps']"
gsettings set org.gnome.desktop.peripherals.keyboard delay "150"
gsettings set org.gnome.desktop.peripherals.keyboard repeat-interval "16"
#setxkbmap dvorak -option ctrl:nocaps && xset r rate 150 64

# Enable natural scrolling, for the touchpad.
gsettings set org.gnome.desktop.peripherals.touchpad natural-scroll true

# Disable mouse acceleration.  c.f. https://askubuntu.com/questions/1065542/how-to-disable-mouse-acceleration-on-ubuntu-18-04
#gsettings set org.gnome.desktop.peripherals.mouse accel-profile "default"
gsettings set org.gnome.desktop.peripherals.mouse accel-profile "flat"

# Configure ".lesskey".
lesskey
