(add-to-list 'treesit-extra-load-path "~/.config/emacs/grammars")
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-hook 'emacs-startup-hook (lambda ()
				(message "Emacs loaded in %s."
                                         (emacs-init-time))))

(setq custom-file "~/.config/emacs/custom.el")
(load custom-file)

(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-deferred-compilation t)
(setq use-package-always-ensure t)
(setq inhibit-startup-message t)

(defun open-config()
  (interactive)
  (find-file "~/.config/emacs/init.el"))

(global-set-key (kbd "C-c fed") 'open-config)

(require 'use-package)

(use-package nord-theme
  :config
  (load-theme 'nord t))

(use-package god-mode
  :config
  (define-key god-local-mode-map (kbd ".") #'repeat)
  (define-key god-local-mode-map (kbd "i") #'god-local-mode)
  (global-set-key (kbd "<escape>") #'god-local-mode))

(use-package which-key
  :config
  (which-key-mode))

(use-package magit
  :commands (magit-status))

(use-package company
  :config
  (global-company-mode))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package projectile
  :config
  ;; Recommended keymap prefix on macOS
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  ;; Recommended keymap prefix on Windows/Linux
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode))
	
(use-package fsharp-mode
  :defer t)

(use-package eglot-fsharp
  :defer t)

(use-package move-text
  :commands (move-text-up move-text-down)
  :init
  (global-set-key (kbd "M-p") 'move-text-up)
  (global-set-key (kbd "M-n") 'move-text-down))

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->")         'mc/mark-next-like-this)
  (global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
  (global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this))

(use-package typescript-ts-mode
  :defer t
  :mode "\\.ts\\'"
  :hook ((typescript-ts-mode . eglot-ensure)))

(use-package vterm)

(fido-mode)
(fido-vertical-mode)
