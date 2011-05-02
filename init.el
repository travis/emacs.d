;; initialize package management

(require 'package)

(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))

(package-initialize)

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit"))

;; assorted requires

(require 'swank-cdt)

;; UI

(require 'color-theme)
(require 'color-theme-solarized)
(color-theme-solarized-dark)

(set-default-font "-apple-inconsolata-medium-r-normal--16-130-72-72-m-130-iso10646-1")

;; Bindings

(keyboard-translate ?\( ?\[)
(keyboard-translate ?\[ ?\()
(keyboard-translate ?\) ?\])
(keyboard-translate ?\] ?\))

;;(keyboard-translate ?\^\( ?\^\[)
;;(keyboard-translate ?\^\) ?\^\])
(keyboard-translate ?\^\[ ?\^\()
(keyboard-translate ?\^\] ?\^\))

;; start the server

(server-start)
