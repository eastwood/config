;;; emacs.el --- Emacs configuration

;;; Commentary:
;; A simple, fast and no-nonsense Emacs configuration reduced down over the years.
;; Mantra of the config:

;; 1. We autoload and byte compile all packages.
;; 2. We subscribe to the way of vim, our flow should be evil.
;; 3. We let LSP handle all our coding needs
;; 4. We value performance over laziness

;;; Code:

(defconst my/WINDOWS (memq window-system '(win32)))
(defconst my/OSX (memq window-system '(ns mac)))

(setq user-full-name "Clint Ryan"
      user-mail-address "clint.ryan3@gmail.com")

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
(setq custom-file "~/.emacs.d/custom.el")
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))

(eval-when-compile
  (require 'package)
  (unless (assoc-default "melpa" package-archives)
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))
  (unless (assoc-default "org" package-archives)
    (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-defer t)
  (setq use-package-verbose t)
  (setq use-package-always-ensure t))

(use-package diminish)
(use-package all-the-icons)
(use-package doom-modeline
  :hook
  (after-init . doom-modeline-init))

(use-package doom-themes
  :init
  (load-theme 'doom-one-light 'doom-one)
  :config
  (setq-default doom-neotree-file-icons t)
  (doom-themes-org-config)
  (doom-themes-neotree-config))

(scroll-bar-mode -1)
(global-display-line-numbers-mode t)
(unless my/OSX (menu-bar-mode -1))
(setq inhibit-startup-message t)
(setq-default indent-tabs-mode nil)
(setq-default line-spacing 5)
(setq ring-bell-function 'ignore)
(xterm-mouse-mode 1)
(tool-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)

(use-package company
  :diminish company-mode
  :init
  (global-company-mode t)
  (setq company-tooltip-align-annotations t))

(use-package evil
  :diminish undo-tree-mode
  :init
  (setq evil-want-keybinding nil)
  (evil-mode 1)
  :config
  (setq evil-want-C-u-scroll t))

(use-package evil-collection
  :custom
  (evil-collection-company-use-tng nil)
  (evil-collection-setup-minibuffer t)
  :after evil
  :init
  (evil-collection-init))

(use-package evil-leader
  :after evil
  :config
  (evil-leader/set-leader "SPC")
  (evil-leader/set-key
    "SPC" 'counsel-M-x
    "[" 'previous-error
    "]" 'next-error
    "bb" 'ivy-switch-buffer
    "bl" 'dired
    "bd" 'kill-buffer
    "bk" 'kill-this-buffer
    "bD" 'kill-other-buffers
    "bn" 'next-buffer
    "bp" 'previous-buffer
    "eb" 'eval-buffer
    "er" 'eval-region
    "fs" 'save-buffer
    "fj" 'open-journal-file
    "fo" 'open-org-file
    "fc" 'open-calendar-file
    "ff" 'counsel-find-file
    "fr" 'counsel-recentf
    "fed" 'open-config-file
    "feR" 'reload-config-file
    "ft" 'neotree-toggle
    "is" 'yas-insert-snippet
    "in" 'yas-new-snippet
    "gs" 'magit-status
    "pf" 'projectile-find-file
    "pp" 'projectile-switch-project
    "pt" 'neotree-find-project-root
    "tl" 'toggle-truncate-lines
    "ts" 'eshell
    "qc" 'delete-frame
    "qq" 'save-buffers-kill-terminal
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
  :after evil
  :init
  (global-evil-surround-mode))

(defun my/use-eslint-from-node-modules ()
  "Gets eslint exe from local path."
  (let (eslint)
    (setq eslint (projectile-expand-root "node_modules/eslint/bin/eslint.js"))
    (setq-default flycheck-javascript-eslint-executable eslint)))

(use-package flycheck
  :commands (projectile-switch-project)
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checker 'javascript-jshint)
  (setq-default flycheck-disabled-checker 'json-jsonlist)
  (add-hook 'js2-mode-hook #'my/use-eslint-from-node-modules)
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(when my/OSX
  (add-to-list 'default-frame-alist
               '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist
               '(ns-appearance . dark))) ;; or dark - depending on your theme

  (use-package xclip
    :init
    (xclip-mode))

(when my/OSX
  (use-package exec-path-from-shell
    :init
    (exec-path-from-shell-initialize)))

(defun neotree-find-project-root()
  "Find the root of neotree."
  (interactive)
  (neotree-find (projectile-project-root)))

(defun reload-config-file()
  "Reload our configuration file."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun open-config-file()
  "Open our configuration file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun open-org-file()
  "Open our GTD org file."
  (interactive)
  (find-file "~/Dropbox/notes/gtd.org"))

(defun open-journal-file()
  "Open our journal org file."
  (interactive)
  (find-file "~/Dropbox/notes/journal.org"))

(defun open-calendar-file()
  "Open our calendar file."
  (interactive)
  (find-file "~/Dropbox/notes/calendar.org"))

(defun kill-other-buffers (&optional arg)
  "Kill all other buffers.  If the universal prefix ARG is used then will the windows too."
  (interactive "P")
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? "
                             (buffer-name)))
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (when (equal '(4) arg) (delete-other-windows))
    (message "Buffers deleted!")))

(use-package counsel
  :diminish ivy-mode counsel-mode
  :init
  (evil-leader/set-key
    "sb" 'swiper
    "sg" 'counsel-rg)
  (counsel-mode)
  (ivy-mode))
(use-package counsel-projectile)
(setq ivy-use-virtual-buffers t)
(setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))

(use-package plantuml-mode
  :config
  (setq-default org-plantuml-jar-path (expand-file-name "~/plantuml.jar"))
  (setq plantuml-jar-path (expand-file-name "~/plantuml.jar")))

(setq-default ruby-flymake-use-rubocop-if-available nil)

(use-package lsp-mode
  :init
  (add-hook 'js2-mode-hook 'lsp)
  (add-hook 'typescript-mode-hook 'lsp)
  (setq lsp-auto-guess-root t)
  (setq lsp-auto-configure nil)
  (setq lsp-prefer-flymake nil)
  (require 'lsp-clients))

(use-package company-lsp
  :after company
  :init
  (add-to-list 'company-backends 'company-lsp))

(use-package slime
  :config
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (add-to-list 'slime-contribs 'slime-fancy 'slime-repl))

(use-package csharp-mode)

(use-package omnisharp
  :config
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-to-list 'company-backends 'company-omnisharp))

(use-package json-mode
  :init
  (evil-leader/set-key-for-mode 'json-mode
    "m=" 'json-pretty-print-buffer))

(use-package js2-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(setq js-indent-level 2)

(use-package typescript-mode
  :init
  (setq-default typescript-indent-level 2)
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode)))

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode))

(use-package flycheck-rust
  :commands (rust-mode))

(use-package racer
  :commands (rust-mode)
  :config
  (evil-define-key 'insert rust-mode-map
    (kbd "TAB") 'company-indent-or-complete-common)
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

(setq-default css-indent-offset 2)

(use-package yaml-mode)

(use-package web-mode
  :config
  (defun my-web-mode-hook ()
    "Hooks for Web mode. Adjust indents"
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-attr-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq-default css-indent-offset 2))
  (add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode))
  (add-hook 'web-mode-hook  'my-web-mode-hook))

(use-package magit
  :commands magit-status
  :config
  (use-package evil-magit
    :init
    (evil-magit-init)))

(use-package markdown-mode
  :config
  (setq-default markdown-split-window-direction 'right))

(use-package neotree
  :commands (projectile-switch-project)
  :config
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

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (evil-leader/set-key
    "oc" 'org-capture
    "oa" 'org-agenda)

  (evil-leader/set-key-for-mode 'org-mode
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

  (evil-define-key 'normal org-mode-map
    ">" 'org-shiftmetaright
    "<" 'org-shiftmetaleft
    "c" 'org-toggle-checkbox
    "t" 'org-todo
    (kbd "TAB") 'org-cycle
    "gs" 'org-goto)

  (evil-leader/set-key-for-mode 'org-capture-mode
    "c" 'org-capture-finalize
    "k" 'org-capture-kill)

  (setq-default org-display-inline-images t)
  (setq-default org-redisplay-inline-images t)
  (setq org-startup-with-inline-images "inlineimages")
  (defvar +org-babel-languages
    '(emacs-lisp
      plantuml
      shell
      ))

  (org-babel-do-load-languages
   'org-babel-load-languages
   (cl-loop for sym in +org-babel-languages
            collect (cons sym t)))

  (setq org-use-speed-commands t)
  (setq org-directory "~/Dropbox/notes")
  (setq org-default-notes-file (concat org-directory "/gtd.org"))
  (define-key global-map "\C-cc" 'org-capture)
  (setq org-global-properties '(("Effort_ALL". "0 0:10 0:20 0:30 1:00 2:00 3:00 4:00 6:00 8:00")))
  (setq org-columns-default-format '"%25ITEM %10Effort(Est){+} %TODO %TAGS")
  (org-agenda-files '"~/Dropbox/notes/gtd.org")
  (setq org-tag-alist
        '((:startgroup . nil)
          (:endgroup . nil)
          ("WORK" . ?w) ("HOME" . ?h) ("WORK" . ?w) ("COMPUTER" . ?l) ("GOALS" . ?g) ("READING" . ?r) ("PROJECT" . ?p)))
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
                '(("t" "Todo" entry (file+headline "~/Dropbox/notes/gtd.org" "Inbox")
                   "* TODO %?\n:CREATED: %T\n" :prepend T)
                  ("e" "Event" entry (file "~/Dropbox/notes/calendar.org")
                   "* %?\n%T" :prepend T)
                  ("i" "Ideas" entry (file+headline "~/Dropbox/notes/gtd.org" "Ideas")
                   "* %?\n%T" :prepend T)
                  ("g" "Goals" entry (file+headline "~/Dropbox/notes/gtd.org" "Goals")
                   "* %?\n%T" :prepend T)
                  ("j" "Journal" entry (file+datetree "~/Dropbox/notes/journal.org")
                   "* %?\nEntered on %U\n  %i\n  %a"))))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package projectile
  :diminish projectile-mode
  :commands (projectile-switch-project)
  :config
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy)
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (projectile-mode 1))

(use-package smartparens)

(use-package yasnippet
  :commands (yas-insert-snippet)
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode))

(defun renew-dhcp ()
  "Renews my DHCP lease on windows."
  (interactive)
  (when my/WINDOWS (eshell-command "ipconfig /renew")))

(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)
(toggle-frame-maximized)
(server-start)

;;; init ends here
