(add-to-list 'load-path "~/.config/emacs/eastwood")
(add-to-list 'treesit-extra-load-path "~/.config/emacs/grammars")
(add-hook 'emacs-startup-hook (lambda ()
                                (message "Emacs loaded in %s."
                                         (emacs-init-time))))
(setq custom-file "~/.config/emacs/eastwood/custom.el")

;; Require all of our packages


(require 'rc)
(rc/require
 'no-littering
 'which-key
 'company
 'exec-path-from-shell
 'evil
 'projectile)


(require 'ui)
(require 'languages)
(require 'util)

(add-hook 'after-init-hook '(lambda ()
			      (projectile-mode)
			      (which-key-mode)
			      (global-company-mode)
			      (require 'treesit)
			      (require 'evil)
			      (require 'keybindings)))
