(setq user-full-name "Clinton Ryan"
      user-mail-address "hello@clintonryan.com")

(defconst my/TERM    (memq window-system '(nil)))
(defconst my/WSL     (and (eq system-type 'gnu/linux)
                          (getenv "WSLENV")))

(require 'package)
(require 'use-package)

(setq use-package-always-ensure t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs loaded in %s." (emacs-init-time))))

;; Load custom file
(setq custom-file "~/.config/emacs/custom.el")

;; General Editor Settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default standard-indent 2)

;; Interface
(set-face-attribute 'default nil :family "RobotoMono Nerd Font" :height 120 :weight 'normal :width 'normal)
(set-fontset-font t 'symbol "Apple Color Emoji")
(pixel-scroll-precision-mode t)
(global-display-line-numbers-mode t)
(fido-vertical-mode t)
(fido-mode t)
(electric-pair-mode t)

;; Variables and Warnings
(setq inhibit-startup-message t)
(setq-default dired-kill-when-opening-new-dired-buffer t)
(setq auto-save-file-name-transforms `((".*" "~/.config/emacs/autosaves/" t)))
(setq backup-directory-alist `(("." . "~/.config/emacs/_backups/")))
(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-deferred-compilation t)
(setq warning-minimum-level :error)
(setq ring-bell-function 'ignore)
(setq visible-bell t)
(setq tramp-default-method "sshx")

;; OS Specific Settings
(when my/WSL
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "wslview"))

(unless my/TERM
  (global-set-key (kbd "C-z") 'undo)
  (global-set-key (kbd "C-S-z") 'undo-redo))

;; Package Configuration
(use-package god-mode
 :config
 (setq god-mode-enable-function-key-translation nil)
 (setq god-exempt-major-modes '(vterm-mode info-mode compilation-mode))
 (define-key god-local-mode-map (kbd ".") #'repeat)
 (define-key god-local-mode-map (kbd "i") #'god-mode-all)
 (define-key god-local-mode-map (kbd "[") #'backward-paragraph)
 (define-key god-local-mode-map (kbd "]") #'forward-paragraph)
 
 ;; this is a nice addition to making sure that the cursor changes for visual help
 (defun my/god-mode-update-cursor-type ()
   (setq god-mode-enable-function-key-translation nil)
   (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

 (add-hook 'post-command-hook #'my/god-mode-update-cursor-type))

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
  :custom
  (git-link-open-in-browser t))

(use-package corfu
  :custom
  (corfu-auto t)
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  :init
  (global-corfu-mode))

(use-package exec-path-from-shell
  :hook (after-init . exec-path-from-shell-initialize))

(use-package perspective
  :init
  (setq persp-suppress-no-prefix-key-warning t)
  (setq persp-initial-frame-name "config")
  :config
  (persp-mode))

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
  :custom
  (typescript-indent-level 2))

(use-package json-ts-mode
  :mode "\\.json\\'")

(use-package go-ts-mode
  :mode "\\.go\\'"
  :hook ((go-ts-mode . eglot-ensure))
  :custom
  (go-ts-mode-indent-offset 2))

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
  :custom
  (org-directory "~/Workspace/github.com/eastwood/notes")
  (org-agenda-files (list org-directory))
  (org-confirm-babel-evaluate nil)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ruby . t)
     (shell . t))))

(use-package ein
  :custom
  (ein:output-area-inlined-images t))

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el" :rev :newest :branch "main")
  :custom
  (copilot-indent-offset-warning-disable t)
  :config
  (add-hook 'prog-mode-hook 'copilot-mode)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

(use-package gptel
  :vc (:url "https://github.com/karthink/gptel" :rev :newest)
  :init
  (setq auth-sources '("~/.authinfo"))
  (setq gptel-api-key (auth-source-pick-first-password :host "api.openai.com")))

;; Custom Functions
(defun my/wsl-copy (start end)
  "Copy region to Windows clipboard."
  (interactive "r")
  (call-process-region start end "clip.exe" nil 0))

(defun wsl-clipboard-to-string ()
  "Return Windows clipboard as string."
  (let ((coding-system-for-read 'dos))
    (substring				; remove added trailing \n
     (shell-command-to-string
      "powershell.exe -Command Get-Clipboard") 0 -1)))

(defun my/wsl-paste (arg)
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
  (let* ((project (project-current))
         (name (if project
                   (file-name-nondirectory (directory-file-name (project-root project)))
                 "default-project"))) ; fallback if no project is found
    (browse-url (concat "https://buildkite.com/nib-health-funds-ltd/" name))))

(defun my/open-config()
  (interactive)
  (find-file "~/.config/emacs/init.el"))

(defun my/kill-this-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

;; Project configuration
(eval-after-load "dired"
  '(progn
     (put 'dired-find-alternate-file 'disabled nil)
     (define-key dired-mode-map (kbd "w") 'wdired-change-to-wdired-mode)))

(setq webjump-sites
      '(("Nib Github" .
         [simple-query "github.com"
                       "https://github.com/search?type=repositories&q=org%3Anib-group+"
                       #1=""])
        ("Nib JIRA" .
         [simple-query "nibgroup.atlassian.net"
                       "https://nibgroup.atlassian.net/browse/"
                       #1=""])
        ))

;; Keybindings
(global-set-key (kbd "C-`") #'vterm)
(global-set-key (kbd "C-x *") #'isearch-forward-symbol-at-point)
(global-set-key (kbd "C-x k") #'my/kill-this-buffer)
(global-set-key (kbd "C-x f") #'project-find-file)
(global-set-key (kbd "C-S-f") #'project-find-regexp)
(global-set-key (kbd "C-S-c") #'my/wsl-copy)
(global-set-key (kbd "C-S-v") #'my/wsl-paste)

;; for god-mode
(global-set-key (kbd "C-x C-0") #'delete-window)
(global-set-key (kbd "C-x C-1") #'delete-other-windows)
(global-set-key (kbd "C-x C-2") #'split-window-below)
(global-set-key (kbd "C-x C-3") #'split-window-right)
(global-set-key (kbd "C-x C-o") #'other-window)
(global-set-key (kbd "<escape>") (lambda () (interactive) (god-local-mode t)))

(global-set-key (kbd "C-.") #'eglot-code-actions)
(global-set-key (kbd "M-<up>") #'backward-paragraph)
(global-set-key (kbd "M-<down>") #'forward-paragraph)
(global-set-key (kbd "C-=") #'er/expand-region)
(global-set-key (kbd "C-j") #'join-line)
(global-set-key (kbd "M-p") #'flymake-goto-prev-error)
(global-set-key (kbd "M-n") #'flymake-goto-next-error)

(define-prefix-command 'my/editor-map)
(define-key my/editor-map (kbd "r") #'eglot-rename)
(define-key my/editor-map (kbd "g") #'gptel)
(define-key my/editor-map (kbd "*") #'mc/mark-all-dwim)
(define-key my/editor-map (kbd "l") #'mc/edit-beginnings-of-lines)
(define-key my/editor-map (kbd "c") #'my/open-config)

;; Function binds for my maps
(global-set-key (kbd "C-c SPC") #'my/editor-map)
(global-set-key (kbd "<f2>") #'my/editor-map)
(global-set-key (kbd "<f5>") #'toggle-frame-maximized)
(global-set-key (kbd "<f12>") project-prefix-map)

(load custom-file)
