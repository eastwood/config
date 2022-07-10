;;; Commentary:
 
;; A simple, fast and no-nonsense Emacs configuration reduced down over the years.
;; Mantra of the config:

;; 1. Look good, be evil.
;; 2. LSP is king.
;; 3. Performance > readability.

;;; Code:
(defconst my/WINDOWS (memq window-system '(w32)))
(defconst my/TERM    (memq window-system '(nil)))
(defconst my/OSX     (memq window-system '(ns mac)))
(defconst my/WSL     (memq window-system '(x  nil)))
(defconst my/GTK     (memq window-system '(pgtk)))

(defconst my/CUSTOM-FILE-PATH "~/.emacs.d/custom.el")
(defconst my/CONFIG-FILE "~/.emacs.d/init.el")
(defconst my/ORG-PATH (cond (my/WSL "/mnt/c/users/clint/iCloudDrive/Documents/notes")
                            (my/GTK "/mnt/c/users/clint/iCloudDrive/Documents/notes")
                            (t "~/Documents/notes")))

(setq gc-cons-threshold most-positive-fixnum)
(setq read-process-output-max (* 1024 1024))
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
(setq custom-file my/CUSTOM-FILE-PATH)
(setq-default exec-path-from-shell-check-startup-files nil)
(setq backup-directory-alist `(("." . "/home/eastwd/.emacs.d/backups")))
(auto-save-visited-mode)

(load-file my/CUSTOM-FILE-PATH)

(fset 'yes-or-no-p 'y-or-n-p)
(add-hook 'prog-mode-hook (lambda ()
                            (electric-pair-local-mode)
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

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s."
                     (emacs-init-time))))

(use-package diminish)

(use-package evil
  :hook (after-init . evil-mode)
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-set-undo-system 'undo-redo)
  (evil-define-key 'normal 'global "j" 'evil-next-visual-line)
  (evil-define-key 'normal 'global "k" 'evil-previous-visual-line)
  (setq evil-want-C-u-scroll t))

(use-package evil-leader
  :after evil
  :config
  (evil-leader/set-leader "SPC")
  (evil-leader/set-key
    "SPC" 'counsel-M-x
    "=" 'what-cursor-position ; cool for finding faces
    "[" 'flycheck-previous-error
    "]" 'flycheck-next-error
    "!" 'flycheck-list-errors
    "." 'flycheck-display-error-at-point
    "bb" 'ivy-switch-buffer
    "bl" 'dired
    "bd" 'kill-buffer
    "bk" 'kill-this-buffer
    "bD" 'kill-other-buffers
    "bn" 'next-buffer
    "bp" 'previous-buffer
    "ca" 'lsp-execute-code-action
    "eb" 'eval-buffer
    "ee" 'eval-last-sexp
    "er" 'eval-region
    "fs" 'save-buffer
    "fow" (open-org-file "work.org")
    "foh" (open-org-file "personal.org")
    "fd" 'open-org-directory
    "fed" 'open-config-file
    "feR" 'reload-config-file
    "ff" 'counsel-find-file
    "fr" 'counsel-recentf
    "ft" 'neotree-find
    "is" 'yas-insert-snippet
    "in" 'yas-new-snippet
    "gs" 'magit-status
    "pf" 'counsel-projectile-find-file
    "pp" 'projectile-switch-project
    "sg" 'counsel-projectile-rg
    "sw" 'my/search-current-word
    "sb" 'swiper
    "pr" 'counsel-register
    "pt" 'neotree-find-project-root
    "pw" 'window-configuration-to-register
    "tl" 'toggle-truncate-lines
    "ts" 'open-shell
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

(use-package evil-collection
  :after evil
  :init
  (evil-collection-init '(magit dired)))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package all-the-icons)

(use-package doom-themes
  :init
  (cond (my/TERM (load-theme 'dark+ t))
        (t (load-theme 'doom-solarized-dark t)))
  :config
  (setq-default doom-themes-neotree-theme "doom-colors")
  (setq-default doom-themes-neotree-file-icons t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(use-package solaire-mode
  :hook (after-init . solaire-global-mode))

(defun neotree-find-project-root()
  "Find the root of neotree."
  (interactive)
  (neotree-find (projectile-project-root)))

(use-package neotree
  :after evil
  :commands (neotree-find)
  :config
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
  (setq neo-window-width 50
        neo-toggle-window-keep-p t
        neo-autorefresh t
        neo-window-fixed-size nil
        neo-default-system-application "open"
        neo-smart-open t))

(use-package company
  :commands (counsel-M-x)
  :config
  (global-company-mode t)
  (setq company-tooltip-align-annotations t))

(use-package restclient
  :mode ("\\.rest" . restclient-mode)
  :mode ("\\.http" . restclient-mode))

(use-package evil-terminal-cursor-changer
  :requires my/TERM
  :after evil
  :init
  (evil-terminal-cursor-changer-activate))

(use-package plantuml-mode
  :config
  (setq org-plantuml-jar-path "~/plantuml.jar")
  (setq plantuml-default-exec-mode "server"))

(when my/WSL
  (customize-set-variable 'browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe")
  (customize-set-variable 'browse-url-generic-args     '("/c" "start"))
  (customize-set-variable ' browse-url-browser-function #'browse-url-generic))

(defun my/open-git()
  "Opens git in browser."
  (interactive)
  (call-interactively 'git-link))

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

(use-package plantuml-mode)

(defun open-org-directory ()
  (interactive)
  (counsel-find-file my/ORG-PATH))


(use-package evil-surround
  :after evil
  :hook (evil-mode . global-evil-surround-mode))

(use-package flycheck
  :after evil
  :config
  (global-flycheck-mode)
  (flymake-mode-off)
  (setq flycheck-ruby-rubocop-executable "~/.rbenv/shims/rubocop")
  (evil-define-key 'normal flycheck-mode-map
    (kbd "gh") 'flycheck-display-error-at-point)
  (setq-default flycheck-disabled-checkers '(javascript-jshint json-jsonlist))
  (flycheck-add-mode 'javascript-eslint 'web-mode))

;; (when (or my/OSX my/GTK)
;;  (setq explicit-shell-file-name "/usr/bin/zsh")
;;  (use-package exec-path-from-shell
;;    :config
;;    (exec-path-from-shell-initialize)))

(use-package xclip
             :if my/WSL
             :init
             (xclip-mode))

(defun reload-config-file()
  "Reload our configuration file."
  (interactive)
  (load-file my/CONFIG-FILE))

(defun open-config-file()
  "Open our configuration file."
  (interactive)
  (find-file my/CONFIG-FILE))

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

(defun my/switch-project ()
  (projectile-find-file)
  (neotree-projectile-action)
  (other-window 1))

(defun open-shell ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (eshell '(4))))

(use-package projectile
  :diminish projectile-mode
  :commands (projectile-switch-project projectile-project-root)
  :config
  (define-key global-map (kbd "<f12>") 'open-shell)
  (projectile-mode)
  (setq projectile-switch-project-action 'my/switch-project)
  (setq projectile-enable-caching t
        projectile-completion-system 'ivy)
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))

(use-package counsel-projectile)

(use-package persp-mode)

(use-package lsp-mode
  :hook ((typescript-mode . lsp-deferred)
         (web-mode . lsp-deferred)
         (js2-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :init
  (setq exec-path (append exec-path '("/home/eastwd/.nvm/versions/node/v14.17.6/bin")))
  :config
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-auto-configure t)
  :custom
  ;; https://github.com/emacs-lsp/lsp-mode/wiki/LSP-ESlint-integration 
  (lsp-eslint-server-command 
   '("node" 
     "/home/eastwd/.emacs.d/eslint-server/server/out/eslintServer.js" 
     "--stdio"))
  ;; put the log files to stderr
  (lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/dev/stderr"))
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package jest)

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
  :mode "\\.json"
  :config
  (evil-leader/set-key-for-mode 'json-mode
    "m=" 'json-pretty-print-buffer))

(use-package add-node-modules-path)

(use-package js2-mode
  :mode "\\.js\\'"
  :diminish js2-mode
  :config
  (setq js2-basic-offset 2)
  (customize-set-variable 'js2-basic-offset 2)
  (customize-set-variable 'js2-indent-level 2))

(use-package rjsx-mode
  :mode "\\.jsx\\'"
  :diminish rjsx-mode)

(use-package ejc-sql)

(use-package typescript-mode
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :diminish typescript-mode
  :config
  (setq-default typescript-indent-level 2))

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package yaml-mode
  :mode "\\.yaml\\'")

(use-package web-mode
  :mode ("\\.cshtml\\'" "\\.jsx\\'")
  :custom
  (web-mode-markup-indent-offset 2
   web-mode-attr-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   css-indent-offset 2)
  :config
  (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "#50FA7B")
  (add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode)))

(use-package git-link
  :config
  (setq git-link-open-in-browser t))

(use-package magit
  :commands magit-status)

(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (evil-define-key 'normal markdown-mode-map
    "c" 'markdown-toggle-gfm-checkbox)
  (setq-default markdown-split-window-direction 'right))

(defun my/org-archive ()
  "Archive and save file."
  (interactive)
  (org-archive-subtree)
  (save-some-buffers 'always (lambda ()
                               (or (string-match-p "inbox.org_archive" buffer-file-name)
                                   (string-match-p "work.org_archive" buffer-file-name)
                                   (string-match-p "personal.org_archive" buffer-file-name)))))

(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "λ")
                                       ("#+END_SRC" . "λ")
                                       ("#+begin_src" . "λ")
                                       ("#+end_src" . "λ")))

(setq prettify-symbols-unprettify-at-point 'right-edge)
(add-hook 'org-mode-hook 'prettify-symbols-mode)

(set-face-attribute 'default nil
                    :family "Hack"
                    :height 120
                    :weight 'normal
                    :width 'normal)

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (add-to-list 'evil-emacs-state-modes 'nov-mode))

(use-package org
  :after evil
  :mode ("\\.org\\'" . org-mode)
  :hook (org-mode . org-indent-mode)
  :config
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'variable-pitch-mode)
  (setq org-confirm-babel-evaluate nil)
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

  (evil-leader/set-key-for-mode 'org-journal-mode
    "mls" 'org-store-link
    "mlp" 'org-insert-last-stored-link)

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

  (defvar +org-babel-languages '(emacs-lisp
                                 js
                                 plantuml
                                 restclient
                                 shell))

  (org-babel-do-load-languages 'org-babel-load-languages
                               (cl-loop for sym in +org-babel-languages
                                        collect (cons sym t)))

  (setq org-use-speed-commands t)
  (setq org-directory my/ORG-PATH)
  (setq org-default-notes-file (concat org-directory "/work.org"))
  (setq org-global-properties '(("Effort_ALL". "0 0:10 0:20 0:30 1:00 2:00 3:00 4:00 6:00 8:00")))
  (setq org-columns-default-format '"%25ITEM %10Effort(Est){+} %TODO %TAGS")
  (setq org-agenda-files (directory-files-recursively my/ORG-PATH "\.org$" t))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))

  (setq org-tag-alist
        '((:startgroup . nil)
          (:endgroup . nil)
          ("WORK" . ?w) ("HOME" . ?h) ("WORK" . ?w) ("COMPUTER" . ?l) ("GOALS" . ?g) ("READING" . ?r) ("PROJECT" . ?p)))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "REPORT(r)" "BUG(b)" "KNOWN-CAUSE(k)" "|" "DONE(d)" "NOT-DOING(n)")))

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
                `(("w" "Work" entry (file+headline ,(get-org-file "work.org") "Inbox") "* TODO %?%^g\n%T" :prepend T)
                  ("h" "Home" entry (file+headline ,(get-org-file "personal.org") "Inbox") "* TODO %?%^g\n%T" :prepend T))))

(use-package ob-restclient)
(use-package ox-gfm)
(use-package htmlize)

(use-package org-journal
  :commands (org-journal-new-entry)
  :init
  (evil-leader/set-key
    "fj" (lambda ()
           (interactive)
           (org-journal-new-entry t)))
  :config
  (setq org-journal-dir (concat my/ORG-PATH "/journal"))
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-date-format "%A, %d %B %Y"))

(use-package org-bullets
             :unless my/WSL
             :hook (org-mode . org-bullets-mode))

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
  :hook (evil-mode . which-key-mode))

;; Reduce the gc to idle times
(use-package gcmh
  :hook (after-init . gcmh-mode))

(require 'server nil t)
(use-package server
  :if window-system
  :init
  (when (not (server-running-p server-name))
    (server-start)))
;;; init ends here
