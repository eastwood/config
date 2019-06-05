;;; emacs.el --- Emacs configuration

;;; Commentary:

;; A simple, fast and no-nonsense Emacs configuration reduced down over the years.
;; Mantra of the config:

;; 1. We autoload and byte compile all packages.
;; 2. We subscribe to the way of vim, our flow should be evil.
;; 3. We let LSP handle all our coding needs
;; 4. We value performance over laziness

;;; Code:

(defconst my/WINDOWS (memq window-system '(w32)))
(defconst my/OSX (memq window-system '(ns mac)))
(defconst my/CUSTOM-FILE-PATH "~/.emacs.d/custom.el")
(defconst my/CONFIG-FILE "~/.emacs.d/init.el")
(defconst my/ORG-PATH "~/Dropbox/notes")
(defconst my/PLANTUML_JAR "~/plantuml.jar")

(setq user-full-name "Clint Ryan"
      user-mail-address "clint.ryan3@gmail.com")

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
(setq custom-file my/CUSTOM-FILE-PATH)
(load-file my/CUSTOM-FILE-PATH)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/\\1" t)))

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
  (setq use-package-always-defer t)
  (setq use-package-verbose t)
  (setq use-package-always-ensure t))

(use-package diminish)
(use-package all-the-icons)
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package doom-themes
  :init
  (load-theme 'doom-one 't)
  :config
  (setq-default doom-neotree-file-icons t)
  (doom-themes-org-config)
  (doom-themes-neotree-config))

(scroll-bar-mode -1)
(global-display-line-numbers-mode t)
(global-hl-line-mode t)
(unless my/OSX (menu-bar-mode -1))
(setq inhibit-startup-message t)
(setq-default indent-tabs-mode nil)
(setq-default line-spacing nil)
(setq ring-bell-function 'ignore)
(xterm-mouse-mode 1)
(tool-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)

(use-package company
  :diminish company-mode
  :init
  (global-company-mode t)
  (setq company-tooltip-align-annotations t))

(use-package restclient)
(use-package ob-restclient)

(use-package evil
  :diminish undo-tree-mode
  :init
  (setq evil-want-keybinding nil)
  (evil-mode 1)
  :config
  (evil-define-key 'normal 'global "j" 'evil-next-visual-line)
  (evil-define-key 'normal 'global "k" 'evil-previous-visual-line)
  (setq evil-want-C-u-scroll t))

(use-package evil-collection
  :custom
  (evil-collection-company-use-tng nil)
  (evil-collection-setup-minibuffer t)
  (evil-collection-outline-bind-tab-p nil)
  :after evil
  :init
  (evil-collection-init))

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
    "ee" 'eval-last-sexp
    "er" 'eval-region
    "fs" 'save-buffer
    "fj" (open-org-file "journal.org")
    "fo" (open-org-file "gtd.org")
    "fc" (open-org-file "calendar.org")
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

(defun my/use-tslint-from-node-modules ()
  "Gets eslint exe from local path."
  (let (tslint)
    (setq tslint (projectile-expand-root "node_modules/tslint/bin/tslint"))
    ;; (setq-default flycheck-tslint-args `("--project" ,(projectile-expand-root "tsconfig.json") "--type-check"))
    (setq-default flycheck-typescript-tslint-executable tslint)))

(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checker 'javascript-jshint)
  (setq-default flycheck-disabled-checker 'json-jsonlist)
  (add-hook 'js2-mode-hook #'my/use-eslint-from-node-modules)
  (add-hook 'typescript-mode-hook #'my/use-tslint-from-node-modules)
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(when my/OSX
  (use-package xclip
    :init
    (xclip-mode))
  (add-to-list 'default-frame-alist
               '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist
               '(ns-appearance . dark))) ;; or dark - depending on your theme

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
  :init
  (evil-leader/set-key
    "sw" 'my/search-current-word
    "sb" 'swiper
    "sg" 'counsel-rg)
  (counsel-mode)
  (counsel-projectile-mode)
  (ivy-mode))

(use-package counsel-projectile
  :commands counsel-projectile-mode
  :config
  (counsel-projectile-mode t))

(setq ivy-use-virtual-buffers t)
(setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))

(defun setup-tide-mode ()
  "Set up tide mode."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;; Specifically for typescript as lsp mode isn't working well
(use-package tide
  :hook (typescript-mode . setup-tide-mode)
  :init
  (setq company-tooltip-align-annotations t))

(use-package lsp-mode
  :hook
  (js2-mode . lsp)
  (ruby-mode . lsp)
  :commands lsp
  :config
  (setq-default ruby-flymake-use-rubocop-if-available nil)
  (setq lsp-auto-guess-root t)
  (setq lsp-auto-configure t)
  (setq lsp-prefer-flymake t)
  (evil-define-key 'normal lsp-mode-map
    "gh" 'lsp-describe-thing-at-point)
  (require 'lsp-clients))

(use-package company-lsp
  :after lsp-mode)

(electric-pair-mode)

(use-package slime
  :config
  (evil-leader/set-key-for-mode 'lisp-mode
    "eb" 'slime-eval-buffer)
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (when my/WINDOWS (setq inferior-lisp-program "sbcl.exe"))
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
  :diminish js2-mode)
(use-package rjsx-mode
  :diminish rjsx-mode)

(setq js2-basic-offset 2)
(setq js-indent-level 2)

(use-package typescript-mode
  :diminish typescript-mode
  :init
  (setq-default typescript-indent-level 2)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode)))

(use-package flycheck-rust)

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

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
  (setq neo-autorefresh t)
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

(use-package htmlize)

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
  :init
  (evil-leader/set-key
    "oc" 'org-capture
    "oa" 'org-agenda)
  :config
  (add-hook 'org-mode-hook (lambda ()
                             "Beautify Org Checkbox Symbol"
                             (push '("[ ]" . "☐") prettify-symbols-alist)
                             (push '("[X]" . "☑" ) prettify-symbols-alist)
                             (push '("[-]" . "❍" ) prettify-symbols-alist)
                             (push '("#+BEGIN_SRC" . "λ" ) prettify-symbols-alist)
                             (push '("#+END_SRC" . "λ" ) prettify-symbols-alist)
                             (prettify-symbols-mode)))
  (defun my/org-archive ()
    "Archives and saves file."
    (interactive)
    (org-archive-subtree)
    (save-some-buffers 'always (lambda ()
                                 (string-match-p "gtd.org_archive" buffer-file-name))))

  (evil-leader/set-key-for-mode 'org-mode
    "ma" 'my/org-archive
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
    (kbd "C-j") 'org-next-item
    (kbd "C-k") 'org-previous-item
    (kbd "TAB") 'org-cycle
    "gs" 'org-goto)

  (evil-leader/set-key-for-mode 'org-capture-mode
    "c" 'org-capture-finalize
    "k" 'org-capture-kill)

  (setq org-startup-with-inline-images "inlineimages")
  (setq org-confirm-babel-evaluate nil)
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

  (setq-default org-plantuml-jar-path (expand-file-name my/PLANTUML_JAR))
  (defvar +org-babel-languages '(emacs-lisp
                                 plantuml
                                 restclient
                                 shell))

  (org-babel-do-load-languages 'org-babel-load-languages
                               (cl-loop for sym in +org-babel-languages
                                        collect (cons sym t)))

  (setq org-use-speed-commands t)
  (setq org-directory my/ORG-PATH)
  (setq org-default-notes-file (concat org-directory "/gtd.org"))
  (define-key global-map "\C-cc" 'org-capture)

  (define-key global-map [?\s-F] 'replace-regexp)
  (define-key global-map [?\s-l] 'goto-line)

  (setq org-global-properties '(("Effort_ALL". "0 0:10 0:20 0:30 1:00 2:00 3:00 4:00 6:00 8:00")))
  (setq org-columns-default-format '"%25ITEM %10Effort(Est){+} %TODO %TAGS")
  (org-agenda-files (concat org-directory "/gtd.org"))
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
                `(("t" "Todo" entry (file+headline ,(get-org-file "gtd.org") "Inbox")
                   "* TODO %?\n:CREATED: %T\n" :prepend T)
                  ("a" "Action" entry (file+headline ,(get-org-file "gtd.org") "Actions")
                   "* %?\n%T" :prepend T)
                  ("i" "Ideas" entry (file+headline ,(get-org-file "gtd.org") "Ideas")
                   "* %?\n%T" :prepend T)
                  ("g" "Goals" entry (file+headline ,(get-org-file "gtd.org") "Goals")
                   "* %?\n%T" :prepend T)
                  ("j" "Journal" entry (file+datetree ,(get-org-file "journal.org"))
                   "* %?\nEntered on %U\n  %i\n  %a"))))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package projectile
  :diminish projectile-mode
  :commands (projectile-switch-project)
  :init
  (define-key global-map [?\s-P] 'projectile-switch-project)
  :config
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy)
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (projectile-mode 1))

(use-package expand-region
  :init
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package yasnippet
  :commands (yas-insert-snippet)
  :init
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
