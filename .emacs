; https://proofgeneral.github.io/
; To install:
; > Then, run M-x package-refresh-contents RET followed by M-x package-install
; > RET proof-general RET to install and byte-compile proof-general.
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives
               (cons "melpa" (concat proto "://melpa.org/packages/")) t))
(package-initialize)
