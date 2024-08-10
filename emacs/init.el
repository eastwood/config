(setq user-full-name "Clinton Ryan"
      user-mail-address "hello@clintonryan.com")

(defconst my/WINDOWS (memq window-system '(w32)))
(defconst my/TERM    (memq window-system '(nil)))
(defconst my/OSX     (memq window-system '(ns mac)))
(defconst my/WSL     (memq window-system '(x nil)))
(defconst my/GTK     (memq window-system '(pgtk)))

(require 'use-package)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs loaded in %s."
                     (emacs-init-time))))

(set-face-attribute 'default nil :family "RobotoMono Nerd Font" :height 140 :weight 'normal :width 'normal)
(set-fontset-font t 'symbol "Apple Color Emoji")

(setq custom-file "~/.config/emacs/custom.el")
(setq auto-save-file-name-transforms
      `((".*" "~/.config/emacs/autosaves/" t)))

(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-deferred-compilation t)
(setq inhibit-startup-message t)
(setq visible-bell t)
(setq ring-bell-function 'ignore)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

(fido-vertical-mode t)
(fido-mode t)
(electric-pair-mode t)

(unless my/TERM
  (global-set-key (kbd "C-z") 'undo)
  (global-set-key (kbd "C-S-z") 'undo-redo))

(use-package god-mode
 :config
 (setq god-mode-enable-function-key-translation nil)
 (setq god-exempt-major-modes '(vterm-mode info-mode))
 (define-key god-local-mode-map (kbd ".") #'repeat)
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
  :config
  (setq tab-always-indent 'complete)
  (global-corfu-mode))

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(use-package projectile
  :commands (projectile-switch-project)
  :init
  (projectile-mode)
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-S-f") 'projectile-ripgrep)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package perspective
  :init
  (setq persp-suppress-no-prefix-key-warning t)
  :config
  (persp-mode))

(use-package fsharp-mode
  :defer t
  :config
  (require 'eglot-fsharp)
  (define-key fsharp-mode-map (kbd "M-RET") 'fsharp-eval-phrase)
  (define-key fsharp-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key fsharp-mode-map (kbd "M-p") 'flymake-goto-prev-error)
  (require 'eglot-fsharp)
  (setq inferior-fsharp-program "dotnet fsi --readline-"))

(use-package move-text
  :commands (move-text-up move-text-down))

(use-package expand-region
  :commands (er/expand-region))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package typescript-ts-mode
  :mode "\\.ts\\'"
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
  :config
  (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp"))
  :hook ((ruby-ts-mode . eglot-ensure)))

(use-package python-mode
  :mode "\\.py\\'"
  :hook ((python-mode . eglot-ensure)))

(use-package multiple-cursors)
(use-package yaml-ts-mode)
(use-package vterm)
(use-package doom-modeline)
(use-package rg)

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :hook (prog-mode . copilot-mode)
  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (setq copilot-indent-offset-warning-disable t)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

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

(global-set-key (kbd "C-S-c") 'wsl-copy-region-to-clipboard)
(global-set-key (kbd "C-S-v") 'wsl-paste-from-clipboard)
(global-set-key (kbd "C-c fed") 'open-config)
(global-set-key (kbd "C-x C-0") #'delete-window)
(global-set-key (kbd "C-x C-1") #'delete-other-windows)
(global-set-key (kbd "C-x C-2") #'split-window-below)
(global-set-key (kbd "C-x C-3") #'split-window-right)
(global-set-key (kbd "C-x C-o") #'other-window)
(global-set-key (kbd "<escape>") (lambda () (interactive) (god-local-mode t)))
(global-set-key (kbd "C-.") #'eglot-code-actions)
(global-set-key (kbd "M-<up>") 'backward-paragraph)
(global-set-key (kbd "M-<down>") 'forward-paragraph)
(global-set-key (kbd "C-<up>") 'move-text-up)
(global-set-key (kbd "C-<down>") 'move-text-down)
(global-set-key (kbd "s-<up>") 'backward-paragraph)
(global-set-key (kbd "s-<down>") 'forward-paragraph)
(global-set-key (kbd "C-M-<up>")  'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-<down>") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-<left>") 'mc/mark-all-like-this)
(global-set-key (kbd "C-M-<right>") 'mc/skip-to-next-like-this)
(global-set-key (kbd "<f12>") 'persp-switch)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "M-n") 'flymake-goto-next-error)
(global-set-key (kbd "M-p") 'flymake-goto-prev-error)
(global-set-key (kbd "<f10>") 'vterm)

(load custom-file)
(load-theme 'leuven-dark t)
