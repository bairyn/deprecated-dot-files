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
		package { 'htop': ensure => installed, }
		#package { 'fontutils': ensure => installed, }
		package { 'findutils': ensure => installed, }
		package { 'smartmontools': ensure => installed, }
		package { 'ncdu': ensure => installed, }
		package { 'tree': ensure => installed, }
		package { 'xsel': ensure => installed, }
		package { 'xclip': ensure => installed, }
		package { 'lynx': ensure => installed, }
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
	}

	class games {
		package { 'neverball': ensure => installed, }
		package { 'crawl': ensure => installed, }
		package { 'wesnoth': ensure => installed, }
	}
}
