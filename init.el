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
				   'gist
				   'durendal
				   'slime-repl
				   'idle-highlight
				   'starter-kit
				   'clojure-mode
				   'starter-kit-lisp
				   'clojure-test-mode
				   'paredit
                                   'solarized-theme
				   'swank-cdt
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

(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit"))
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit/rspec-mode"))
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit/auto-complete"))

;; assorted requires

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(require 'rspec-mode)

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
(global-set-key (kbd "C->") 'indent-rigidly)
(global-set-key (kbd "C-x g") 'magit-status)

(defun rec ()
  (interactive)
  (with-current-buffer "*compilation*"
    (recompile)))
(global-set-key (kbd "C-x C-j") 'rec)

;; start the server

(server-start)

;;;;; stuff that requires packages to be loaded from elpa ;;;;;

;; On your first run, this should pull in all the base packages.
(defun init-packages ()
  (when (esk-online?)
    (unless package-archive-contents (package-refresh-contents))
    (starter-kit-elpa-install)))
;;(add-hook 'after-init-hook 'init-packages)

(defun requires-elpa ())
(add-hook 'after-init-hook 'requires-elpa)

;; auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(add-hook 'slime-mode-hook 'set-up-slime-ac)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 2)
 '(column-number-mode t)
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes (quote ("485737acc3bedc0318a567f1c0f5e7ed2dfde3fb" default)))
 '(exec-path (quote ("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/bin" "/Users/travis/bin")))
 '(global-whitespace-mode t)
 '(grep-command "grep -nH -e ")
 '(grep-files-aliases (quote (("rails" . "*.rb *.erb *.js *.css *.yml *.yaml *.sass") ("all" . "* .*") ("el" . "*.el") ("ch" . "*.[ch]") ("c" . "*.c") ("cc" . "*.cc *.cxx *.cpp *.C *.CC *.c++") ("cchh" . "*.cc *.[ch]xx *.[ch]pp *.[CHh] *.CC *.HH *.[ch]++") ("hh" . "*.hxx *.hpp *.[Hh] *.HH *.h++") ("h" . "*.h") ("l" . "[Cc]hange[Ll]og*") ("m" . "[Mm]akefile*") ("tex" . "*.tex") ("texi" . "*.texi") ("asm" . "*.[sS]"))))
 '(grep-find-command nil)
 '(grep-find-ignored-files (quote (".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo")))
 '(grep-find-template "find . <X> -type f <F> -exec grep <C> -nH -e <R> {} /dev/null \\;")
 '(grep-highlight-matches (quote auto))
 '(grep-template "grep <X> <C> -nH -e <R> <F>")
 '(grep-use-null-device nil)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(ruby-insert-encoding-magic-comment nil)
 '(safe-local-variable-values (quote ((whitespace-action (quote nil)) (nil rvm-use "1.9.2" "lagunitas") (ruby-compilation-executable . "ruby") (ruby-compilation-executable . "ruby1.8") (ruby-compilation-executable . "ruby1.9") (ruby-compilation-executable . "rbx") (ruby-compilation-executable . "jruby") (whitespace-line-column . 80) (lexical-binding . t))))
 '(show-trailing-whitespace t)
 '(whitespace-action (quote (auto-cleanup)))
 '(whitespace-line-column 100)
 '(yas/root-directory (quote ("~/.emacs.d/yas")) nil (yasnippet)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-item-highlight ((t (:background "#073642"))) t))

(defvar crm-separator ",")
(setq redisplay-dont-pause t)

;; Set to the location of your Org files on your local system
(setq org-directory "~/org")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/MobileOrg")

(setenv "PATH" (concat "/Users/travis/bin:" (getenv "PATH")))
(setq evernote-ruby-command "/usr/local/bin/ruby")
