include bairyn::all

class bairyn {
	#include bairyn::all

	class all {
		include bairyn::meta
		include bairyn::dev_base
		include bairyn::utils
		include bairyn::cli
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
		package { 'rust': ensure => installed, }
		package { 'cargo': ensure => installed, }
		package { 'ruby': ensure => installed, }
		package { 'python3': ensure => installed, }
		package { 'lua': ensure => installed, }
		package { 'R': ensure => installed, }
	}

	class utils {
		package { 'htop': ensure => installed, }
		#package { 'fontutils': ensure => installed, }
		package { 'findutils': ensure => installed, }
		package { 'smartmontools': ensure => installed, }
		package { 'ncdu': ensure => installed, }
		package { 'tree': ensure => installed, }
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
		package { 'openssh': ensure => installed, }
		package { 'rsync': ensure => installed, }
		package { 'nmap': ensure => installed, }
		package { 'iptables': ensure => installed, }
	}

	class applications {
		package { 'firefox': ensure => installed, }
		package { 'chromium': ensure => installed, }
		package { 'anki': ensure => installed, }
		package { 'keepassx': ensure => installed, }
		package { 'feh': ensure => installed, }

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
	}
}
