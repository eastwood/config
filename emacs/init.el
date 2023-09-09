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
  (load-theme 'nord))

; Use this when we're really frustrated default bindings
(use-package evil)

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

(use-package fsharp-mode)
(use-package eglot-fsharp)

(use-package projectile
  :config
  ;; Recommended keymap prefix on macOS
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  ;; Recommended keymap prefix on Windows/Linux
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode))

(use-package typescript-ts-mode
  :mode "\\.ts\\'"
  :hook ((typescript-ts-mode . eglot-ensure)))

(fido-mode)
(fido-vertical-mode)
