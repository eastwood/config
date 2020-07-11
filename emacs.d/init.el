;;; emacs.el --- Emacs configuration

;;; Commentary:

;; A simple, fast and no-nonsense Emacs configuration reduced down over the years.
;; Mantra of the config:

;; 1. Look good, be evil.
;; 2. LSP is king.
;; 3. Performance > readability.

;;; Code:
(defconst my/WINDOWS (memq window-system '(w32)))
(defconst my/OSX (memq window-system '(ns mac nil)))
(defconst my/CUSTOM-FILE-PATH "~/.emacs.d/custom.el")
(defconst my/CONFIG-FILE "~/.emacs.d/init.el")
(defconst my/ORG-PATH "~/Dropbox/notes")

(setq user-full-name "Clint Ryan"
      user-mail-address "clint.ryan3@gmail.com")

(setq gc-cons-threshold most-positive-fixnum)
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
(setq custom-file my/CUSTOM-FILE-PATH)
(setq-default exec-path-from-shell-check-startup-files nil)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))

(load-file my/CUSTOM-FILE-PATH)

(setq-default gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3") ;; Updates TLS to force 1.3+

(set-face-background 'vertical-border "black")
(set-face-foreground 'vertical-border (face-background 'vertical-border))

(eval-when-compile
  (require 'package)
  (unless (assoc-default "elpa" package-archives)
    (add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t))
  (unless (assoc-default "melpa" package-archives)
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
  (unless (assoc-default "org" package-archives)
    (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-verbose t
        use-package-always-defer t
	use-package-always-ensure t))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(xterm-mouse-mode 1)
(tool-bar-mode -1)
(electric-pair-mode)
(fset 'yes-or-no-p 'y-or-n-p)
(add-hook 'prog-mode-hook (lambda ()
                            (display-line-numbers-mode t)
                            (hl-line-mode t)))

(unless (display-graphic-p)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

(setq-default inhibit-startup-message t
              indent-tabs-mode nil
              line-spacing nil
              truncate-lines nil
              ring-bell-function 'ignore)

(use-package diminish)

(use-package solaire-mode
  :after doom-themes
  :hook (minibuffer-setup . solaire-mode-in-minibuffer)
  :init
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package all-the-icons)

(use-package doom-themes
  :init
  (load-theme 'doom-gruvbox t)
  :config
  (setq-default doom-themes-neotree-theme "doom-colors")
  (doom-themes-neotree-config)
  (setq-default doom-themes-neotree-file-icons t)
  (doom-themes-org-config))

(defun neotree-find-project-root()
  "Find the root of neotree."
  (interactive)
  (neotree-find (projectile-project-root)))

(use-package neotree
  :commands (neotree-find)
  :init
  (evil-leader/set-key
    "pt" 'neotree-find-project-root
    "ft" 'neotree-find)
  :config
  (setq neo-toggle-window-keep-p t)
  (define-key evil-motion-state-map "\\" 'neotree-toggle)
  (evil-define-key 'normal neotree-mode-map
    (kbd "TAB") 'neotree-enter
    "H" 'neotree-hidden-file-toggle
    "i" 'neotree-enter-horizontal-split
    "s" 'neotree-enter-vertical-split
    "q" 'neotree-hide
    (kbd "RET") 'neotree-enter)

  (evil-leader/set-key-for-mode 'neotree-mode
    "mo" 'neotree-open-file-in-system-application
    "md" 'neotree-delete-node
    "mr" 'neotree-rename-node
    "mc" 'neotree-create-node)

  (setq neo-theme (if (display-graphic-p) 'icons 'nerd))
  (setq neo-window-fixed-size nil)
  (setq neo-smart-open t))

(setq neo-window-width 40)
(setq neo-default-system-application "open")

(use-package company
  :commands (counsel-M-x)
  :config
  (global-company-mode t)
  (setq company-tooltip-align-annotations t))

(use-package restclient
  :mode ("\\.rest" . restclient-mode)
  :mode ("\\.http" . restclient-mode))

(use-package evil
  :hook (after-init . evil-mode)
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-define-key 'normal 'global "j" 'evil-next-visual-line)
  (evil-define-key 'normal 'global "k" 'evil-previous-visual-line)
  (setq evil-want-C-u-scroll t))

(use-package evil-leader
  :after evil
  :config
  (defmacro open-org-file (filename)
    "Macro for opening files in org directory with FILENAME filepath."
    `(lambda ()
       (interactive)
       (find-file (concat my/ORG-PATH "/" ,filename) nil)))

  (defun get-org-file (filename)
    "Macro for opening files in org directory with FILENAME filepath."
    (concat my/ORG-PATH "/" filename))

  (defun open-org-directory ()
    (interactive)
    (counsel-find-file my/ORG-PATH))

  (evil-leader/set-leader "SPC")
  (evil-leader/set-key
    "SPC" 'counsel-M-x
    "[" 'previous-error
    "]" 'next-error
    "!" 'flycheck-list-errors
    "." 'flycheck-display-error-at-point
    "bb" 'ivy-switch-buffer
    "bl" 'dired
    "bd" 'kill-buffer
    "bk" 'kill-this-buffer
    "bD" 'kill-other-buffers
    "bn" 'next-buffer
    "bp" 'previous-buffer
    "eb" 'eval-buffer
    "ee" 'eval-last-sexp
    "er" 'eval-region
    "fs" 'save-buffer
    "fo" (open-org-file "gtd.org")
    "fd" 'open-org-directory
    "ff" 'counsel-find-file
    "fr" 'counsel-recentf
    "fed" 'open-config-file
    "feR" 'reload-config-file
    "is" 'yas-insert-snippet
    "in" 'yas-new-snippet
    "gs" 'magit-status
    "pf" 'projectile-find-file
    "tl" 'toggle-truncate-lines
    "ts" 'eshell
    "qc" 'delete-frame
    "qq" 'save-buffers-kill-terminal
    "ww" 'ace-window
    "wc" 'evil-window-delete
    "wo" 'delete-other-windows
    "wj" 'evil-window-down
    "wk" 'evil-window-up
    "wh" 'evil-window-left
    "wl" 'evil-window-right
    "wv" 'evil-window-vsplit
    "ws" 'evil-window-split)
  (global-evil-leader-mode))

(use-package evil-surround
  :hook (evil-mode . global-evil-surround-mode))

(defun my/use-eslint-from-node-modules ()
  "Gets eslint exe from local path."
  (let (eslint)
    (setq eslint (projectile-expand-root "node_modules/eslint/bin/eslint.js"))
    (setq-default flycheck-javascript-eslint-executable eslint)))

(use-package flycheck
  :init
  (add-hook 'js2-mode-hook #'my/use-eslint-from-node-modules)
  (add-hook 'typescript-mode-hook #'my/use-eslint-from-node-modules)
  :config
  (flymake-mode-off)
  (global-flycheck-mode)
  (evil-define-key 'normal flycheck-mode-map
    (kbd "gh") 'flycheck-display-error-at-point)
  (setq-default flycheck-disabled-checker 'javascript-jshint
		flycheck-disabled-checker 'json-jsonlist)
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(when my/OSX
  (setq explicit-shell-file-name "/usr/local/bin/tmux")
  (use-package exec-path-from-shell
    :init
    (exec-path-from-shell-initialize))
  (use-package xclip
    :init
    (xclip-mode)))

(defun reload-config-file()
  "Reload our configuration file."
  (interactive)
  (load-file my/CONFIG-FILE))

(defun open-config-file()
  "Open our configuration file."
  (interactive)
  (find-file my/CONFIG-FILE))

(defun blog-base-url ()
  "Get blog base url."
  (interactive)
  (cond
   ((null my/WINDOWS) "~/Workspace/github.com/eastwood/blog")
   (t "C:/code/blog")))

(defun kill-other-buffers (&optional arg)
  "Kill all other buffers.  If the universal prefix ARG is used then will the windows too."
  (interactive "P")
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? "
			     (buffer-name)))
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (when (equal '(4) arg) (delete-other-windows))
    (message "Buffers deleted!")))

(defun my/search-current-word ()
  "RG with word on current selection."
  (interactive)
  (counsel-rg (current-word)))

(use-package counsel
  :diminish ivy-mode counsel-mode
  :commands (counsel-M-x)
  :config
  (counsel-mode)
  (ivy-mode)
  (global-set-key (kbd "C-s") 'swiper)
  (evil-leader/set-key
    "sw" 'my/search-current-word
    "sb" 'swiper
    "sg" 'counsel-rg)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order))))

(use-package ivy-posframe
  :hook (counsel-mode . ivy-posframe-mode)
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
  (setq ivy-posframe-width 128)
  (setq ivy-posframe-border-width 1)
  (setq ivy-posframe-height-alist '((counsel-rg . 40)
				    (t . 20)))
  (ivy-posframe-mode 1))

(use-package projectile
  :diminish projectile-mode
  :commands (projectile-switch-project projectile-project-root)
  :init
  (evil-leader/set-key
    "pp" 'projectile-switch-project)
  :config
  (projectile-mode)
  (setq projectile-enable-caching t
	projectile-completion-system 'ivy)
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))

(use-package counsel-projectile)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (typescript-mode . lsp-deferred)
  :hook (js2-mode . lsp-deferred)
  :config
  (global-flycheck-mode)
  (flycheck-add-next-checker 'lsp 'javascript-eslint)
  (setq-default lsp-prefer-flymake nil))

(use-package lsp-ui :commands lsp-ui-mode)

(use-package slime
  :config
  (evil-leader/set-key-for-mode 'lisp-mode
    "eb" 'slime-eval-buffer)
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (when my/WINDOWS (setq inferior-lisp-program "sbcl.exe"))
  (add-to-list 'slime-contribs 'slime-fancy 'slime-repl))

(use-package csharp-mode
  :mode "\\.cs\\'")

(use-package json-mode
  :mode "\\.json\\'"
  :config
  (evil-leader/set-key-for-mode 'json-mode
    "m=" 'json-pretty-print-buffer))

(use-package js2-mode
  :mode "\\.js\\'"
  :diminish js2-mode
  :config
  (setq js2-basic-offset 2)
  (setq js-indent-level 2))

(use-package rjsx-mode
  :mode "\\.jsx\\'"
  :diminish rjsx-mode)

(use-package ejc-sql)

(use-package typescript-mode
  :mode "\\.ts"
  :diminish typescript-mode
  :config
  (setq-default typescript-indent-level 2))

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package racer
  :hook
  (rust-mode . racer-mode)
  (racer-mode . eldoc-mode)
  :commands (rust-mode)
  :config
  (evil-define-key 'insert rust-mode-map
    (kbd "TAB") 'company-indent-or-complete-common))

(use-package yaml-mode
  :mode "\\.yaml\\'")

(use-package web-mode
  :mode ("\\.cshtml\\'" "\\.jsx\\'" "\\.tsx\\'")
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-attr-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        css-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode)))

(use-package magit
  :commands magit-status
  :config
  (use-package evil-magit
    :init
    (evil-magit-init)))

(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (setq-default markdown-split-window-direction 'right))

(defun deploy-blog ()
  "Deploy my hugo blog."
  (interactive)
  (let ((blogCommand
	 (cond
	  (my/WINDOWS (format "powershell C:/code/blog/deploy.ps1"))
	  (t (format "cd %s && ./deploy.sh" (blog-base-url))))))
    (async-shell-command blogCommand)))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (setq-default org-pretty-entities t
		org-hide-emphasis-markers t
		org-agenda-block-separator ""
		org-fontify-whole-heading-line t
		org-fontify-done-headline t
		org-fontify-quote-and-verse-blocks t)

  (evil-leader/set-key
    "oc" 'org-capture
    "oa" 'org-agenda)

  (evil-leader/set-key-for-mode 'org-mode
    "ma" 'my/org-archive
    "mi" 'org-insert-structure-template
    "mci" 'org-clock-in
    "mco" 'org-clock-out
    "mt" 'org-set-tags-command
    "md" 'org-deadline
    "me" 'org-set-effort
    "mls" 'org-store-link
    "mlp" 'org-insert-last-stored-link
    "mn" 'org-narrow-to-subtree
    "mr" 'org-refile
    "ms" 'org-schedule
    "mw" 'widen)

  (evil-leader/set-key-for-mode 'org-capture-mode
    "c" 'org-capture-finalize
    "k" 'org-capture-kill)

  (evil-define-key 'normal org-mode-map
    ">" 'org-shiftmetaright
    "<" 'org-shiftmetaleft
    "c" 'org-toggle-checkbox
    "t" 'org-todo
    (kbd "C-j") 'org-next-item
    (kbd "C-k") 'org-previous-item
    (kbd "TAB") 'org-cycle
    "gs" 'org-goto)

  (define-key global-map "\C-cc" 'org-capture)
  (define-key global-map [?\s-F] 'replace-regexp)
  (define-key global-map [?\s-l] 'goto-line)

  (setq org-confirm-babel-evaluate nil)
  (custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "Source Sans Pro" :height 120 :weight light))))
   '(fixed-pitch ((t ( :family "Fira Code" :slant normal :weight normal :height 1 :width normal))))
   '(org-block                 ((t (:inherit fixed-pitch))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-property-value        ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword       ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-tag                   ((t (:inherit (shadow fixed-pitch) :weight bold))))
   '(org-table                 ((t (:inherit fixed-pitch))) t)
   '(org-verbatim              ((t (:inherit (shadow fixed-pitch)))))
   '(org-level-1               ((t (:inherit (outline-1 variable-pitch) :height 1.6))))
   '(org-level-2               ((t (:inherit (outline-2 variable-pitch) :weight normal :height 1.4))))
   '(org-level-3               ((t (:inherit (outline-3 variable-pitch) :weight normal :height 1.2))))
   '(org-indent                ((t (:inherit (org-hide fixed-pitch))))))

  (defvar +org-babel-languages '(emacs-lisp
				 js
				 shell))

  (org-babel-do-load-languages 'org-babel-load-languages
			       (cl-loop for sym in +org-babel-languages
					collect (cons sym t)))

  (setq org-use-speed-commands t)
  (setq org-directory my/ORG-PATH)
  (setq org-default-notes-file (concat org-directory "/inbox.org"))
  (setq org-global-properties '(("Effort_ALL". "0 0:10 0:20 0:30 1:00 2:00 3:00 4:00 6:00 8:00")))
  (setq org-columns-default-format '"%25ITEM %10Effort(Est){+} %TODO %TAGS")
  (setq org-agenda-files (directory-files-recursively my/ORG-PATH "\.org$"))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))

  (setq org-tag-alist
	'((:startgroup . nil)
	  (:endgroup . nil)
	  ("WORK" . ?w) ("HOME" . ?h) ("WORK" . ?w) ("COMPUTER" . ?l) ("GOALS" . ?g) ("READING" . ?r) ("PROJECT" . ?p)))
  (setq org-todo-keywords
	'((sequence "TODO(t)" "IN-PROGRESS(i)" "|" "DONE(d)")
	  (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")))

  (setq-default org-agenda-custom-commands
		'(("g" . "GTD contexts")
		  ("gw" "Work" tags-todo "WORK")
		  ("gc" "Computer" tags-todo "COMPUTER")
		  ("gg" "Goals" tags-todo "GOALS")
		  ("gh" "Home" tags-todo "HOME")
		  ("gt" "Tasks" tags-todo "TASKS")
		  ("G" "GTD Block Agenda"
		   ((tags-todo "WORK")
		    (tags-todo "COMPUTER")
		    (tags-todo "GOALS")
		    (tags-todo "TASKS"))
		   nil)))

  (setq-default org-capture-templates
		`(("t" "Inbox" entry (file ,(get-org-file "inbox.org")) "* TODO %?\n:CREATED: %T\n" :prepend T)
		  ("h" "Home" entry (file ,(get-org-file "home.org")) "* %?\n%T" :prepend T)
		  ("w" "Work" entry (file ,(get-org-file "work.org")) "* %?\n%T" :prepend T)
		  ("j" "Journal" entry (file+datetree ,(get-org-file "journal.org")) "* %?\nEntered on %U\n  %i\n  %a"))))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :config
  (org-indent-mode 1))

(use-package expand-region
  :init
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package yasnippet
  :commands (yas-insert-snippet)
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package emojify
  :config
  (global-emojify-mode t))

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode))

;; Reduce the gc to idle times
(use-package gcmh
  :hook (after-init . gcmh-mode))

(toggle-frame-maximized)

(if (and (fboundp 'server-running-p)
	 (not (server-running-p)))
   (server-start))

;;; init ends here
