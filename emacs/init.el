(setq user-full-name "Clinton Ryan"
      user-mail-address "hello@clintonryan.com")

(defconst my/WINDOWS (memq window-system '(w32)))
(defconst my/TERM    (memq window-system '(nil)))
(defconst my/OSX     (memq window-system '(ns mac)))
(defconst my/WSL     (memq window-system '(x nil)))
(defconst my/GTK     (memq window-system '(pgtk)))

(require 'package)
(require 'use-package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs loaded in %s." (emacs-init-time))))

(set-face-attribute 'default nil :family "RobotoMono Nerd Font" :height 120 :weight 'normal :width 'normal)
(set-fontset-font t 'symbol "Apple Color Emoji")

(setq custom-file "~/.config/emacs/custom.el")
(setq auto-save-file-name-transforms `((".*" "~/.config/emacs/autosaves/" t)))
(setq backup-directory-alist `(("." . "~/.config/emacs/_backups/")))
(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-deferred-compilation t)
(setq use-package-always-ensure t)
(setq inhibit-startup-message t)
(setq visible-bell t)
(setq ring-bell-function 'ignore)
(setq warning-minimum-level :error)
(setq-default dired-kill-when-opening-new-dired-buffer t)

(global-display-line-numbers-mode t)

(fido-vertical-mode t)
(fido-mode t)
(electric-pair-mode t)

(unless my/TERM
  (global-set-key (kbd "C-z") 'undo)
  (global-set-key (kbd "C-S-z") 'undo-redo))

(use-package god-mode
 :config
 (setq god-mode-enable-function-key-translation nil)
 (setq god-exempt-major-modes '(vterm-mode info-mode compilation-mode))
 (define-key god-local-mode-map (kbd ".") #'repeat)
 (define-key god-local-mode-map (kbd "C-<f12>") #'persp-switch)
 (define-key god-local-mode-map (kbd "C-<f10>") #'vterm)
 (define-key god-local-mode-map (kbd "i") #'god-mode-all)
 (define-key god-local-mode-map (kbd "[") #'backward-paragraph)
 (define-key god-local-mode-map (kbd "]") #'forward-paragraph)
 
 ;; this is a nice addition to making sure that the cursor changes for visual help
 (defun my-god-mode-update-cursor-type ()
   (setq god-mode-enable-function-key-translation nil)
   (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

 (add-hook 'post-command-hook #'my-god-mode-update-cursor-type))

(use-package which-key
  :config
  (which-key-mode))

(defun my/kill-magit-buffer ()
  (interactive)
  (magit-mode-bury-buffer 16))

(use-package magit
  :commands (magit-status)
  :config
  (define-key magit-mode-map (kbd "q") 'my/kill-magit-buffer))

(use-package git-link
  :commands (git-link)
  :config
  (setq git-link-open-in-browser t))

(use-package corfu
  :custom
  (corfu-auto t)
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  :init
  (global-corfu-mode))

(use-package exec-path-from-shell
  :hook (after-init . exec-path-from-shell-initialize))

;; When switching projectile projects, also switch to the corresponding perspective
(defun my/persp-switch-project ()
  (let ((persp (projectile-project-name)))
    (when persp
      (persp-switch persp))))

(use-package projectile
  :commands (projectile-switch-project)
  :init
  (projectile-mode)
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-S-f") 'projectile-ripgrep)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  ;; When switching projectile projects, also switch to the corresponding perspective
  (add-hook 'projectile-after-switch-project-hook 'my/persp-switch-project))

(use-package perspective
  :init
  (setq persp-suppress-no-prefix-key-warning t)
  (setq persp-initial-frame-name "config")
  :config
  (persp-mode))

(use-package fsharp-mode
  :defer t
  :config
  (require 'eglot-fsharp)
  (define-key fsharp-mode-map (kbd "M-RET") 'fsharp-eval-phrase)
  (define-key fsharp-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key fsharp-mode-map (kbd "M-p") 'flymake-goto-prev-error)
  (setq inferior-fsharp-program "dotnet fsi --readline-"))

(use-package move-text
  :commands (move-text-up move-text-down)
  :bind (("C-<up>" . move-text-up)
         ("C-<down>" . move-text-down)))

(use-package expand-region
  :commands (er/expand-region))

(use-package ace-window
  :bind (("M-o" . ace-window)))

(use-package ace-link
  :config
  (ace-link-setup-default))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package typescript-ts-mode
  :mode ("\\.ts\\'" "\\.js\\'")
  :hook ((typescript-ts-mode . eglot-ensure))
  :config
  (setq-default typescript-indent-level 2))

(use-package json-ts-mode
  :mode "\\.json\\'")

(use-package go-ts-mode
  :mode "\\.go\\'"
  :hook ((go-ts-mode . eglot-ensure))
  :config
  (setq go-ts-mode-indent-offset 2))

(use-package rust-ts-mode
  :mode "\\.rs\\'"
  :hook ((rust-ts-mode . eglot-ensure)))

(use-package ruby-ts-mode
  :mode "\\.rb\\'"
  :hook ((ruby-ts-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp")))

(use-package inf-ruby)

(use-package python-mode
  :mode "\\.py\\'"
  :hook ((python-ts-mode . eglot-ensure))
  :config
  (setq python-indent-offset 4))

(use-package python-black
  :hook (python-ts-mode . python-black-on-save-mode-enable-dwim))

(use-package pyvenv
  :after python
  :hook ((python-ts-mode . pyvenv-tracking-mode)))

(use-package multiple-cursors)

(use-package yaml-mode)

(use-package vterm
  :hook (vterm-mode . (lambda () (display-line-numbers-mode -1))))

(use-package doom-modeline)

(use-package rg)

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package terraform-mode
  :custom (terraform-indent-level 2)
  :config
  (defun my-terraform-mode-init ()
    (outline-minor-mode 1))
  (add-hook 'terraform-mode-hook 'my-terraform-mode-init))

(use-package editorconfig)

(use-package org
  :init
  (setq org-startup-indented t)
  :config
  (setq org-directory "~/Workspace/github.com/eastwood/notes")
  (setq org-agenda-files (list org-directory))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ruby . t)
     (shell . t))))

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el" :rev :newest :branch "main")
  :config
  (add-hook 'prog-mode-hook 'copilot-mode)
  (setq copilot-indent-offset-warning-disable t)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

(use-package gptel
  :vc (:url "https://github.com/karthink/gptel" :rev :newest)
  :init
  (setq auth-sources '("~/.authinfo"))
  (setq gptel-api-key (auth-source-pick-first-password :host "api.openai.com")))

;; Functions
(defun wsl-copy-region-to-clipboard (start end)
  "Copy region to Windows clipboard."
  (interactive "r")
  (call-process-region start end "clip.exe" nil 0))

(defun wsl-clipboard-to-string ()
  "Return Windows clipboard as string."
  (let ((coding-system-for-read 'dos))
    (substring				; remove added trailing \n
     (shell-command-to-string
      "powershell.exe -Command Get-Clipboard") 0 -1)))

(defun wsl-paste-from-clipboard (arg)
  "Insert Windows clipboard at point. With prefix ARG, also add to kill-ring"
  (interactive "P")
  (let ((clip (wsl-clipboard-to-string)))
    (insert clip)
    (if arg (kill-new clip))))

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

(defun open-config()
  (interactive)
  (find-file "~/.config/emacs/init.el"))

(global-set-key (kbd "C-S-c") #'wsl-copy-region-to-clipboard)
(global-set-key (kbd "C-S-v") #'wsl-paste-from-clipboard)
(global-set-key (kbd "C-c fed") #'open-config)
(global-set-key (kbd "C-x C-0") #'delete-window)
(global-set-key (kbd "C-x C-1") #'delete-other-windows)
(global-set-key (kbd "C-x C-2") #'split-window-below)
(global-set-key (kbd "C-x C-3") #'split-window-right)
(global-set-key (kbd "C-x C-o") #'other-window)
(global-set-key (kbd "<escape>") (lambda () (interactive) (god-local-mode t)))
(global-set-key (kbd "C-.") #'eglot-code-actions)
(global-set-key (kbd "M-<up>") #'backward-paragraph)
(global-set-key (kbd "M-<down>") #'forward-paragraph)
(global-set-key (kbd "s-<up>") #'backward-paragraph)
(global-set-key (kbd "s-<down>") #'forward-paragraph)
(global-set-key (kbd "C-M-<up>") #'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-<down>") #'mc/mark-next-like-this)
(global-set-key (kbd "C-M-<left>") #'mc/mark-all-like-this)
(global-set-key (kbd "C-M-<right>") #'mc/skip-to-next-like-this)
(global-set-key (kbd "<f12>") #'persp-switch)
(global-set-key (kbd "C-=") #'er/expand-region)
(global-set-key (kbd "C-j") #'join-line)
(global-set-key (kbd "M-n") #'flymake-goto-next-error)
(global-set-key (kbd "M-p") #'flymake-goto-prev-error)
(global-set-key (kbd "<f10>") #'vterm)
(global-set-key (kbd "<f5>") #'toggle-frame-maximized)

(load custom-file)

(eval-after-load "dired"
  '(progn
     (put 'dired-find-alternate-file 'disabled nil)
     (define-key dired-mode-map (kbd "w") 'wdired-change-to-wdired-mode)))

;; General Editor Settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default standard-indent 2)
(setq org-confirm-babel-evaluate nil)
