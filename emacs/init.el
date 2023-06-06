(add-to-list 'load-path "~/.config/emacs/eastwood")
(add-to-list 'treesit-extra-load-path "~/.config/emacs/grammars")
(add-hook 'emacs-startup-hook (lambda ()
                                (message "Emacs loaded in %s."
                                         (emacs-init-time))))

(setq custom-file "~/.config/emacs/eastwood/custom.el")
(require 'rc)
(require 'ui)
(require 'languages)
(require 'util)

;; Require all of our packages
(rc/require
 'no-littering
 'which-key
 'company
 'projectile)

;; The GOAT
(which-key-mode)

(add-hook 'after-init-hook '(lambda ()
			      (projectile-mode)
			      (global-company-mode)
			      (require 'treesit)
			      (require 'keybindings)))

