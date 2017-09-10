(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))
(setq custom-file "~/.emacs.d/custom-settings.el")
(load-file "~/Dropbox/Keys/emacs_secret.el")
(load custom-file t)

(setq user-full-name "Clint Ryan"
      user-mail-address "")

(global-linum-mode 1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)
(setq-default indent-tabs-mode nil)
(setq ring-bell-function 'ignore)
(tool-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)

(use-package company
  :ensure t
  :init (add-hook 'prog-mode-hook 'global-company-mode))

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-dark t))

(use-package evil
  :ensure t
  :diminish evil-mode
  :init 
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1))
(use-package evil-leader
  :ensure t
  :config
  (evil-leader/set-leader "SPC")
  (evil-leader/set-key
   "SPC" 'counsel-M-x
   "bb" 'ivy-switch-buffer
   "bd" 'kill-buffer
   "bk" 'kill-this-buffer
   "bD" 'kill-other-buffers
   "bn" 'next-buffer
   "bp" 'previous-buffer
   "eb" 'eval-buffer
   "er" 'eval-region
   "fs" 'save-buffer
   "fo" 'open-org-file
   "ff" 'counsel-find-file
   "fr" 'counsel-recentf
   "fed" 'open-config-file
   "feR" 'reload-config-file
   "tl" 'toggle-truncate-lines
   "wc" 'evil-window-delete
   "ww" 'ace-window
   "wo" 'delete-other-windows
   "wj" 'evil-window-down
   "wk" 'evil-window-up
   "wh" 'evil-window-left
   "wl" 'evil-window-right
   "wv" 'evil-window-vsplit
   "ws" 'evil-window-split)
  (global-evil-leader-mode))
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (setq-default flycheck-disabled-checker 'javascript-jshint)
  (setq-default flycheck-disabled-checker 'json-jsonlist)
  (setq-default flycheck-javascript-eslint-executable "eslint-project-relative")
  (with-eval-after-load 'flycheck
    (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))) (flycheck-add-mode 'javascript-eslint 'web-mode))

(setq-default indent-tabs-mode nil)
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "<f12>") 'ansi-term)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(global-set-key (kbd "s-p") 'counsel-M-x)

;; OSX fix for eslint lookup
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(defun init-install()
  (setq use-package-always-ensure t)
  reload-config-file())

(defun neotree-find-project-root()
  (interactive)
  (if (neo-global--window-exists-p)
      (neotree-hide)
    (let ((origin-buffer-file-name (buffer-file-name)))
      (neotree-find (projectile-project-root))
      (neotree-find origin-buffer-file-name))))

(defun reload-config-file()
  (interactive)
  (org-babel-load-file "~/.emacs.d/org-init.org"))

(defun open-config-file()
  (interactive)
  (find-file "~/.emacs.d/org-init.org"))

(defun open-org-file()
  (interactive)
  (find-file "~/Dropbox/notes/gtd.org"))

(defun kill-other-buffers (&optional arg)
  "Kill all other buffers.
If the universal prefix argument is used then will the windows too."
  (interactive "P")
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? "
                             (buffer-name)))
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (when (equal '(4) arg) (delete-other-windows))
    (message "Buffers deleted!")))

(use-package counsel
  :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-re-builders-alist
   '((t . ivy--regex-ignore-order)))
  :bind
  ("s-f" . swiper))

(use-package omnisharp
  :ensure t
  :config
  (add-hook 'csharp-mode-hook 'omnisharp-mode))

(use-package json-mode
  :ensure t)
(use-package js2-mode
  :ensure t
  :diminish js2-mode
  :config
  (setq js2-basic-offset 2)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode)))
(use-package web-mode
  :ensure t
  :config
  (defun my-web-mode-hook ()
    "Hooks for Web mode. Adjust indents"
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-attr-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2))
  (add-hook 'web-mode-hook  'my-web-mode-hook))
(use-package company-tern
  :ensure t
  :config
  (add-to-list 'company-backends 'company-tern))
(use-package tern
  :ensure t
  :config
  (evil-leader/set-key-for-mode 'js2-mode "mf" 'tern-find-definition)
  (evil-leader/set-key-for-mode 'js2-jsx-mode "mf" 'tern-find-definition)
  (add-hook 'js2-mode-hook 'tern-mode))

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode))

(use-package flycheck-rust
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package racer
  :ensure t
  :config
  (evil-define-key 'insert rust-mode-map
    (kbd "TAB") 'company-indent-or-complete-common)
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

 (use-package cargo
   :ensure t
   :config
   (add-hook 'rust-mode-hook 'cargo-minor-mode)
   (evil-leader/set-key-for-mode 'rust-mode
     "mb" 'cargo-process-build
     "mr" 'cargo-process-run
     "mt" 'cargo-process-test))

(use-package magit
  :ensure t
  :commands magit-status
  :init
  (use-package evil-magit
    :ensure t)
  (evil-leader/set-key
   "gs" 'magit-status))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode))

(use-package neotree
  :ensure t
  :config
  (evil-define-key 'normal neotree-mode-map 
    (kbd "TAB") 'neotree-enter
    "H" 'neotree-hidden-file-toggle
    "q" 'neotree-hide
    (kbd "RET") 'neotree-enter)

  (evil-leader/set-key-for-mode 'neotree-mode 
    "mo" 'neotree-open-file-in-system-application
    "md" 'neotree-delete-node
    "mr" 'neotree-rename-node
    "mc" 'neotree-create-node)

  (setq neo-theme 'nerd)
  (setq neo-window-fixed-size nil)
  (setq neo-smart-open t))
  (setq neo-window-width 40)
  (setq neo-default-system-application "open")

(require 'org-agenda)
(define-key org-agenda-mode-map "c" 'org-agenda-columns)
(setq org-directory "~/Dropbox/notes")
(setq org-default-notes-file (concat org-directory "/gtd.org"))
(define-key global-map "\C-cc" 'org-capture)
(setq org-global-properties '(("Effort_ALL". "0 0:10 0:20 0:30 1:00 2:00 3:00 4:00 6:00 8:00")))
(setq org-columns-default-format '"%25ITEM %10Effort(Est){+} %TODO %TAGS")
(org-agenda-files '"~/Dropbox/notes/gtd.org")
(setq org-tag-alist '((:startgroup . nil)
                      (:endgroup . nil)
                      ("WORK" . ?w) ("HOME" . ?h) ("COMPUTER" . ?l) ("GOALS" . ?g) ("READING" . ?r) ("PROJECT" . ?p)))
(setq org-agenda-custom-commands
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
         nil                      ;; i.e., no local settings
         )))
(evil-leader/set-key
    "oc" 'org-capture
    "oa" 'org-agenda
    "os" 'org-schedule)

(evil-leader/set-key-for-mode 'org-mode
    "mt" 'org-set-tags-command
    "md" 'org-deadline
    "me" 'org-set-effort
    "mn" 'org-narrow-to-subtree
    "mr" 'org-refile
    "mw" 'widen)

(evil-define-key 'normal org-mode-map
  ">" 'org-shiftmetaright
  "<" 'org-shiftmetaleft
  "c" 'org-toggle-checkbox
  "t" 'org-todo
  "gs" 'org-goto
)
(evil-leader/set-key-for-mode 'org-capture-mode "c" 'org-capture-finalize)
(evil-leader/set-key-for-mode 'org-capture-mode "k" 'org-capture-kill)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/notes/gtd.org" "Inbox")
             "* TODO %?\n%T" :prepend T)
        ("i" "Ideas" entry (file+headline "~/Dropbox/notes/gtd.org" "Ideas")
             "* %?\n%T" :prepend T)
        ("g" "Goals" entry (file+headline "~/Dropbox/notes/gtd.org" "Goals")
             "* %?\n%T" :prepend T)
        ("j" "Journal" entry (file+datetree "~/Dropbox/notes/journal.org")
             "* %?\nEntered on %U\n  %i\n  %a")))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(setq org-use-speed-commands t)

(use-package org-gcal
  :ensure t
  :config 
  (setq org-gcal-client-id "95243140966-d8b059mb4ug8a996ecgie0ovkpjmsk7q.apps.googleusercontent.com"
      org-gcal-client-secret '(my/google-secrets())
      org-gcal-file-alist '(("clint.ryan3@gmail.com" .  "~/Dropbox/notes/calendar.org")))
)

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :commands (projectile-find-file projectile-switch-project)
  :init
  (evil-leader/set-key
    "pf" 'projectile-find-file
    "pp" 'projectile-switch-project
    "ft" 'neotree-toggle
    "pt" 'neotree-find-project-root)
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-global-mode))

(use-package smartparens
  :ensure t)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (evil-leader/set-key
   "is" 'yas-insert-snippet
   "in" 'yas-new-snippet)
  (yas-global-mode 1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))
