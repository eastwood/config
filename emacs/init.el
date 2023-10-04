;;; package --- Summary
;;; My nice vanilla config, use as much defaults as possible
;;; Commentary:
;;; Is good
;;; Code
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
	    (message "Emacs loaded in %s."
                     (emacs-init-time))))

;; My prefered font
(set-face-attribute
 'default nil
 :family "RobotoMono Nerd Font"
 :height 140
 :weight 'normal
 :width 'normal)

;; Gotta have that emoji support
(set-fontset-font t 'symbol "Apple Color Emoji")

(setq custom-file "~/.config/emacs/custom.el")
(setq-default native-comp-async-report-warnings-errors nil)
(setq-default native-comp-deferred-compilation t)
(setq use-package-always-ensure t)
(setq inhibit-startup-message t)
(setq visible-bell t)
(setq ring-bell-function 'ignore)

(fido-vertical-mode t)
(fido-mode t)
(electric-pair-mode t)

(unless my/TERM
  (global-set-key (kbd "C-z") 'undo)
  (global-set-key (kbd "C-S-z") 'undo-redo))

(use-package god-mode
  :defines god-local-mode-map god-mode-enable-function-key-translation god-exempt-major-modes god-local-mode
  :functions god-mode-all my-god-mode-update-cursor-type god-local-mode
  :init
  (setq god-mode-enable-function-key-translation nil)
  (setq god-exempt-major-modes '(vterm-mode info-mode))
  :config
  (define-key god-local-mode-map (kbd ".") #'repeat)
  (define-key god-local-mode-map (kbd "i") #'god-mode-all)
  (define-key god-local-mode-map (kbd "[") #'backward-paragraph)
  (define-key god-local-mode-map (kbd "]") #'forward-paragraph)
  ;; this is a nice addition to making sure that the cursor changes for visual help
  (defun my-god-mode-update-cursor-type ()
    (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))
  (add-hook 'post-command-hook #'my-god-mode-update-cursor-type))

(use-package which-key
  :functions which-key-mode
  :config
  (which-key-mode))

(defun my/kill-magit-buffer ()
  "Kill magit buffers and all within project."
  (interactive)
  (setq current-prefix-arg '(16))
  (magit-mode-bury-buffer))

(use-package magit
  :defines magit-mode-map
  :functions magit-mode-bury-buffer
  :commands (magit-status)
  :config
  (define-key magit-mode-map (kbd "q") 'my/kill-magit-buffer))

(use-package git-link
  :custom
  (git-link-open-in-browser t)
  :commands (git-link))

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
  "Open config file."
  (interactive)
  (find-file "~/.config/emacs/init.el"))

(use-package corfu
  :defines corfu-auto
  :functions global-corfu-mode
  :config
  (setq corfu-auto t)
  (setq tab-always-indent 'complete)
  (global-corfu-mode))

(use-package exec-path-from-shell
  :commands (exec-path-from-shell-initialize))

(use-package projectile
  :defines projectile-mode-map
  :functions projectile-mode projectile-project-name
  :commands (projectile-switch-project)
  :config
  (projectile-mode)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-S-f") 'projectile-ripgrep)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package perspective
  :functions persp-mode
  :custom
  (persp-suppress-no-prefix-key-warning t)
  :config
  (persp-mode))

(use-package fsharp-mode
  :mode "\\.fs\\'"
  :mode "\\.fsx\\'")

(use-package eglot-fsharp
  :defer t)

(use-package move-text
  :commands (move-text-up move-text-down))

(use-package expand-region
  :commands (er/expand-region))

(use-package typescript-ts-mode
  :mode "\\.ts\\'"
  :hook ((typescript-ts-mode . eglot-ensure))
  :config
  (setq-default typescript-indent-level 2))

(use-package elfeed
  :defines elfeed-feeds
  :config
  (setq elfeed-feeds
	'(("https://reddit.com/r/programming/.rss" programming)
	  ("https://reddit.com/r/golang/.rss" programming golang)
	  ("https://medium.com/feed/coryodaniel" blog)
	  ("https://reddit.com/r/fsharp/.rss" programming fsharp))))

(use-package multiple-cursors)
(use-package yaml-ts-mode)
(use-package vterm)
(use-package doom-modeline)
(use-package rg)
(use-package markdown-mode)

;; You'll need to download wl-clipboard to get this working for
;; WSL2. Otherwise, there's some really shit freezes and experiences.
;; This will also make it so that have a separate process running
;; Called wl-copy in the background, which you'll need to exit
(defun my/configure-wayland-clipboard()
    (setq wl-copy-process nil)
    (defun wl-copy (text)
      (setq wl-copy-process
	    (make-process :name "wl-copy"
			  :buffer nil
			  :command '("wl-copy" "-f" "-n")
			  :connection-type 'pipe))
      (process-send-string wl-copy-process text)
      (process-send-eof wl-copy-process))
  
  (defun wl-paste ()
    (if (and wl-copy-process (process-live-p wl-copy-process))
	nil ; should return nil if we're the current paste owner
      (shell-command-to-string "wl-paste -n | tr -d \r")))
  (setq interprogram-cut-function 'wl-copy))

(when my/GTK
  (my/configure-wayland-clipboard))

(global-set-key (kbd "C-c fed") 'open-config)
(global-set-key (kbd "C-x C-0") #'delete-window)
(global-set-key (kbd "C-x C-1") #'delete-other-windows)
(global-set-key (kbd "C-x C-2") #'split-window-below)
(global-set-key (kbd "C-x C-3") #'split-window-right)
(global-set-key (kbd "C-x C-o") #'other-window)
(global-set-key (kbd "<escape>") (lambda () (interactive) (god-local-mode t)))
(global-set-key (kbd "C-.") #'eglot-code-actions)
(global-set-key (kbd "M-<up>") 'flymake-goto-prev-error)
(global-set-key (kbd "M-<down>") 'flymake-goto-next-error)
(global-set-key (kbd "C-<up>") 'move-text-up)
(global-set-key (kbd "C-<down>") 'move-text-down)
(global-set-key (kbd "s-<up>") 'backward-paragraph)
(global-set-key (kbd "s-<down>") 'forward-paragraph)
(global-set-key (kbd "C-M-<up>")  'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-<down>") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-<left>") 'mc/mark-all-like-this)
(global-set-key (kbd "C-M-<right>") 'mc/skip-to-next-like-this)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "<f12>") 'persp-switch)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c p p") 'projectile-switch-project)

(load custom-file)
(load-theme 'leuven-dark t)

(provide 'init)
;;; init.el ends here
