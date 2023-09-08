(add-to-list 'load-path "~/.config/emacs/eastwood")
(add-to-list 'treesit-extra-load-path "~/.config/emacs/grammars")
(add-hook 'emacs-startup-hook (lambda ()
                                (message "Emacs loaded in %s."
                                         (emacs-init-time))))

(setq custom-file "~/.config/emacs/custom.el")
(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-deferred-compilation t)
(setq use-package-always-ensure t)
(setq inhibit-startup-message t)

(defun open-config()
  (interactive)
  (find-file "~/.config/emacs/init.el"))

(global-set-key (kbd "C-c fed") 'open-config)

(defun fido-exit-with-input ()
  "Exit minibuffer using the current input (without completing)."
  (interactive)
  (let ((input (minibuffer-contents)))
    (minibuffer-force-complete-and-exit)
    (if (string-empty-p input)
        (keyboard-quit)
      (insert input))))

(require 'use-package)

(use-package evil)
 
(use-package which-key
  :config
  (which-key-mode))

(use-package magit
  :commands (magit-status))

(use-package company
  :config
  (global-company-mode))

(use-package exec-path-from-shell)
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
