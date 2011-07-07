;; initialize package management

(require 'package)

(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))

(package-initialize)

(defvar starter-kit-packages (list 'idle-highlight-mode
                                   'ruby-mode
                                   'inf-ruby
                                   'css-mode
                                   'yaml-mode
                                   'find-file-in-project
                                   'magit
                                   'gist
                                   'durendal
                                   'slime-repl
                                   'idle-highlight
                                   'starter-kit
                                   'clojure-mode
                                   'magit
                                   'starter-kit-lisp
                                   'clojure-test-mode
                                   'paredit
                                   'swank-cdt
                                   'color-theme-solarized
                                   'swank-clojure
                                   'slime)
  "Libraries that should be installed by default.")

(defun starter-kit-elpa-install ()
  "Install all starter-kit packages that aren't installed."
  (interactive)
  (dolist (package starter-kit-packages)
    (unless (or (member package package-activated-list)
                (functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package))))

(defun esk-online? ()
  "See if we're online.

Windows does not have the network-interface-list function, so we
just have to assume it's online."
  ;; TODO how could this work on Windows?
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (some (lambda (iface) (unless (equal "lo" (car iface))
                         (member 'up (first (last (network-interface-info
                                                   (car iface)))))))
            (network-interface-list))
    t))


;; set up dotfiles directory

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit"))

;; assorted requires

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; fonts

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

(global-set-key (kbd "C-#") 'comment-or-uncomment-region)


;; start the server

(server-start)

;;;;; stuff that requires packages to be loaded from elpa ;;;;;

;; On your first run, this should pull in all the base packages.
(defun init-packages ()
  (when (esk-online?)
    (unless package-archive-contents (package-refresh-contents))
    (starter-kit-elpa-install)))
(add-hook 'after-init-hook 'init-packages)

(defun requires-elpa ()
  ;; UI

  (require 'color-theme)
  (require 'color-theme-solarized)
  (color-theme-solarized-dark)

  (require 'swank-cdt))
(add-hook 'after-init-hook 'requires-elpa)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 2)
 '(column-number-mode t)
 '(exec-path (quote ("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/bin" "/Users/travis/bin")))
 '(global-whitespace-mode t)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(ruby-insert-encoding-magic-comment nil)
 '(show-trailing-whitespace t)
 '(whitespace-action (quote (auto-cleanup))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
