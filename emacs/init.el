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
(setq visible-bell t)
(setq ring-bell-function 'ignore)
(electric-pair-mode t)

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
  (global-set-key (kbd "C-x C-0") #'delete-window)

  (defun my-god-mode-update-mode-line ()
    (cond
     (god-mode
      (set-face-attribute 'mode-line nil
                          :foreground "#604000"
                          :background "#ffff0b")
      (set-face-attribute 'mode-line-inactive nil
                          :foreground "#3f3000"
                          :background "#fff3da"))
     (t
      (set-face-attribute 'mode-line nil
			  :foreground "#0a0a0a"
			  :background "#eeeeee")
      (set-face-attribute 'mode-line-inactive nil
			  :foreground "#404148"
			  :background "#efefef"))))
  (add-hook 'post-command-hook #'my-god-mode-update-mode-line)
  ;; this is a nice addition to making sure that the cursor changes for visual help
  (defun my-god-mode-update-cursor-type ()
    (setq god-mode-enable-function-key-translation nil)
    (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

  (add-hook 'post-command-hook #'my-god-mode-update-cursor-type))

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

(use-package corfu
  :config
  (global-set-key (kbd "C-.") #'complete-symbol)
  (setq tab-always-indent 'complete)
  (global-corfu-mode))

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

(use-package perspective
  :init
  (global-set-key (kbd "<f12>") 'persp-switch)
  (setq persp-suppress-no-prefix-key-warning t)
  :config
  (persp-mode))

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
  :mode "\\.ts\\'"
  :hook ((typescript-ts-mode . eglot-ensure))
  :config
  (setq-default typescript-indent-level 2))

(use-package vterm)

(fido-mode)
(fido-vertical-mode)
(load-theme 'leuven-dark t)

(use-package doom-modeline
  :init
  (doom-modeline-mode 1))

(defconst my/WINDOWS (memq window-system '(w32)))
(defconst my/TERM    (memq window-system '(nil)))
(defconst my/OSX     (memq window-system '(ns mac)))
(defconst my/WSL     (memq window-system '(x  nil)))
(defconst my/GTK     (memq window-system '(pgtk)))

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

(my/configure-wayland-clipboard)
