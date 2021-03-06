# Run this file with "puppet apply manifests/default.pp", to apply it with
# puppet.
# vim: noet
include bairyn::all

class bairyn {
	#include bairyn::all

	class all {
		include bairyn::meta
		include bairyn::dev_base
		include bairyn::utils
		include bairyn::cli
		include bairyn::network
		include bairyn::applications
		include bairyn::games
		include bairyn::config
	}

	class meta {
		$puppet = $::operatingsystem ? {
			'fedora'    => 'puppet',
			'ubuntu'    => 'puppet',
			'archlinux' => 'puppet',
			default     => 'puppet',
		}

		package { $puppet:
			ensure => installed,
		}

		case $::operatingsystem {
			'fedora': {
			}
			'ubuntu': {
				package { 'aptitude': ensure => installed, }
				package { 'apt-file': ensure => installed, }
			}
			'archlinux': {
			}
			default: {
			}
		}
	}

	class dev_base {
		package { 'git': ensure => installed, }
		package { 'coreutils': ensure => installed, }
		package { 'binutils': ensure => installed, }
		package { 'make': ensure => installed, }
		package { 'cmake': ensure => installed, }
		package { 'elfutils': ensure => installed, }
		package { 'gdb': ensure => installed, }
		package { 'gcc': ensure => installed, }
		package { 'sharutils': ensure => installed, }
		package { 'valgrind': ensure => installed, }
		package { 'bash': ensure => installed, }
		package { 'bc': ensure => installed, }
		package { 'autoconf': ensure => installed, }
		package { 'diffutils': ensure => installed, }
		package { 'ed': ensure => installed, }
		package { 'ghc': ensure => installed, }
		package { 'cabal-install': ensure => installed, }
		package { 'cargo': ensure => installed, }
		package { 'ruby': ensure => installed, }
		package { 'python3': ensure => installed, }
		package { 'binwalk': ensure => installed, }
		package { 'exuberant-ctags': ensure => installed, }
		package { 'flex': ensure => installed, }
		package { 'bison': ensure => installed, }
		package { 'libelf-dev': ensure => installed, }
		# Packages for kernel dev on Ubuntu (https://wiki.ubuntu.com/KernelTeam/GitKernelBuild)
		#package { 'git': ensure => installed, }  # (Already in manifest.)
		package { 'build-essential': ensure => installed, }
		package { 'kernel-package': ensure => installed, }
		package { 'fakeroot': ensure => installed, }
		package { 'libncurses5-dev': ensure => installed, }
		package { 'libssl-dev': ensure => installed, }
		package { 'ccache': ensure => installed, }
		package { 'libelf-dev': ensure => installed, }
		package { 'moreutils': ensure => installed, }
		package { 'clang': ensure => installed, }
		package { 'gnuplot': ensure => installed, }
		package { 'gnuplot-doc': ensure => installed, }

		case $::operatingsystem {
			'fedora': {
				package { 'R': ensure => installed, }
			}
			'ubuntu': {
				package { 'r-base': ensure => installed, }
			}
			'archlinux': {
				package { 'R': ensure => installed, }
			}
			default: {
				package { 'R': ensure => installed, }
			}
		}

		case $::operatingsystem {
			'fedora': {
				package { 'lua': ensure => installed, }
			}
			'ubuntu': {
				package { 'lua5.3': ensure => installed, }
				package { 'lua5.2': ensure => installed, }
				package { 'lua5.1': ensure => installed, }
				package { 'lua50': ensure => installed, }
			}
			'archlinux': {
				package { 'lua': ensure => installed, }
			}
			default: {
				package { 'lua': ensure => installed, }
			}
		}

		$rust = $::operatingsystem ? {
			'fedora'    => 'rust',
			'ubuntu'    => 'rustc',
			'archlinux' => 'rust',
			default     => 'rust',
		}

		package { $rust: ensure => installed, }
	}

	class utils {
		package { 'parallel': ensure => installed, }
		package { 'htop': ensure => installed, }
		package { 'iotop': ensure => installed, }
		package { 'glances': ensure => installed, }
		package { 'jnettop': ensure => installed, }  # on arch
		package { 'ntop': ensure => installed, }  # on arch
		package { 'nethogs': ensure => installed, }  # on arch
		package { 'iftop': ensure => installed, }  # on arch
		package { 'iptraf-ng': ensure => installed, }  # on arch
		package { 'dstat': ensure => installed, }  # on arch
		package { 'sysstat': ensure => installed, }  # on arch
		package { 'xkbset': ensure => installed, }  # on arch (AUR), for BounceKeys.
		#package { 'fontutils': ensure => installed, }
		package { 'findutils': ensure => installed, }
		package { 'smartmontools': ensure => installed, }
		package { 'ncdu': ensure => installed, }
		package { 'tree': ensure => installed, }
		package { 'xsel': ensure => installed, }
		package { 'xclip': ensure => installed, }
		package { 'lynx': ensure => installed, }
		package { 'whois': ensure => installed, }
		package { 'wget': ensure => installed, }
		package { 'curl': ensure => installed, }
		package { 'xdotool': ensure => installed, }
		package { 'scrot': ensure => installed, }
		package { 'watchdog': ensure => installed, }
		package { 'xinetd': ensure => installed, }
		#package { 'lrzsz': ensure => installed, }  # rb etc. don't seem to respond to SIGINT; don't install if not needed for convenience.
	}

	class cli {
		package { 'zsh': ensure => installed, }
		package { 'tmux': ensure => installed, }
		package { 'screen': ensure => installed, }
		package { 'emacs': ensure => installed, }

		$vim = $::operatingsystem ? {
			'fedora'    => 'gvim',
			'ubuntu'    => 'vim-gtk3',
			'archlinux' => 'gvim',
			default     => 'gvim',
		}
		package { $vim: ensure => installed, }
	}

	class network {
		package { 'rsync': ensure => installed, }
		package { 'nmap': ensure => installed, }
		package { 'iptables': ensure => installed, }

		case $::operatingsystem {
			'fedora': {
				package { 'openssh': ensure => installed, }
			}
			'ubuntu': {
				package { 'openssh-client': ensure => installed, }
				package { 'openssh-server': ensure => installed, }
			}
			'archlinux': {
				package { 'openssh': ensure => installed, }
			}
			default: {
				package { 'openssh': ensure => installed, }
			}
		}
	}

	class gui {
		package { 'dconf-tools': ensure => installed, }
		package { 'arandr': ensure => installed, }
	}

	class applications {
		package { 'firefox': ensure => installed, }
		package { 'anki': ensure => installed, }
		package { 'keepassx': ensure => installed, }
		package { 'feh': ensure => installed, }
		package { 'gnome-tweak-tool': ensure => installed, }
		package { 'evince': ensure => installed, }
		package { 'okular': ensure => installed, }
		package { 'mupdf': ensure => installed, }
		package { 'mupdf-tools': ensure => installed, }
		package { 'mplayer': ensure => installed, }
		package { 'vlc': ensure => installed, }
		package { 'contextfree': ensure => installed, }
		package { 'coq': ensure => installed, }
		package { 'coq-doc': ensure => installed, }
		package { 'coqide': ensure => installed, }
		# Proof general: coq for emacs.
		# c.f. https://proofgeneral.github.io/ https://coq.inria.fr/tutorial-nahas
		package { 'proofgeneral': ensure => installed, }
		package { 'proofgeneral-doc': ensure => installed, }
		package { 'rlwrap': ensure => installed, }
		package { 'qemu': ensure => installed, }

		$chromium = $::operatingsystem ? {
			'fedora'    => 'chromium',
			'ubuntu'    => 'chromium-browser',
			'archlinux' => 'chromium',
			default     => 'chromium',
		}
		package { $chromium: ensure => installed, }

		case $::operatingsystem {
			'fedora': {
				package { 'google-chrome-stable': ensure => installed, }
			}
			'ubuntu': {
				# TODO: Either in non-standard repo or outside.
			}
			'archlinux': {
				# TODO AUR: google-chrome
			}
			default: {
				# TODO: can you emit an info/notice message here?
			}
		}

		# Latex.
		package { 'texlive': ensure => installed, }
		package { 'texlive-latex-base': ensure => installed, }
		package { 'python-pygments': ensure => installed, }  # For "minted" package.
		                                                     # https://tex.stackexchange.com/a/116599
	}

	class games {
		package { 'neverball': ensure => installed, }
		package { 'crawl': ensure => installed, }
		package { 'wesnoth': ensure => installed, }
	}

	class config {
		$sysrq_enable_all = "# Generated by puppet manifest in dot-files.\nkernel.sysrq = 1\n"
		file { '/etc/sysctl.d/70-sysrq-enable-all.conf':
			ensure => 'file',
			checksum => 'sha256',
			content => "${sysrq_enable_all}",
			mode => '0664'
		}
	}
}
