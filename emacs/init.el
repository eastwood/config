(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-hook 'emacs-startup-hook (lambda ()
				(message "Emacs loaded in %s."
                                         (emacs-init-time))))

(set-face-attribute 'default nil
                    :family "RobotoMono Nerd Font"
                    :height 120
                    :weight 'normal
                    :width 'normal)

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

(use-package god-mode
  :config
  (setq god-exempt-major-modes nil)
  (setq god-exempt-predicates nil)
  (define-key god-local-mode-map (kbd ".") #'repeat)
  (define-key god-local-mode-map (kbd "i") #'god-mode-all)
  (define-key god-local-mode-map (kbd "[") #'backward-paragraph)
  (define-key god-local-mode-map (kbd "]") #'forward-paragraph)
  (global-set-key (kbd "<escape>") #'god-local-mode)
  (global-set-key (kbd "C-x C-1") #'delete-other-windows)
  (global-set-key (kbd "C-x C-2") #'split-window-below)
  (global-set-key (kbd "C-x C-3") #'split-window-right)
  (global-set-key (kbd "C-x C-0") #'delete-window))

(use-package which-key
  :config
  (which-key-mode))

(use-package magit
  :commands (magit-status))

(use-package git-link
  :commands (git-link)
  :config
  (setq git-link-open-in-browser t))

(defun my/open-jira()
  "Open JIRA in browser."
  (interactive)
  (let (name (magit-get-current-branch))
    (browse-url (concat "https://jira.nib.com.au/browse/" name))))

(defun my/open-buildkite()
  "Open Buildkite in browser."
  (interactive)
  (let ((name (projectile-project-name)))
    (browse-url (concat "https://buildkite.com/nib-health-funds-ltd/" name))))

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
  (global-set-key (kbd "M-<up>") 'move-text-up)
  (global-set-key (kbd "M-<down>") 'move-text-down))

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-S-<up>")         'mc/mark-previous-like-this)
  (global-set-key (kbd "C-S-<down>")         'mc/mark-next-like-this)
  (global-set-key (kbd "C-S-<left>")     'mc/mark-all-like-this)
  (global-set-key (kbd "C-S-<right>")        'mc/skip-to-next-like-this))

(use-package typescript-ts-mode
  :defer t
  :mode "\\.ts\\'"
  :hook ((typescript-ts-mode . eglot-ensure)))

(use-package vterm)
(use-package all-the-icons)

(fido-mode)
(fido-vertical-mode)
