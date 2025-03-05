;; Notes
;; Windows will need to install ripgrep + xargs

(setq user-full-name "Clinton Ryan"
      user-mail-address "hello@clintonryan.com")

(defconst my/TERM   (eq window-system nil))
(defconst my/WSL     (and (eq system-type 'gnu/linux)
                          (getenv "WSLENV")))

(require 'package)
(require 'use-package)

(setq use-package-always-ensure t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(add-hook 'emacs-startup-hook
          (lambda ()
	          (message "Emacs loaded in %s." (emacs-init-time))))

(defun my/get-config-dir()
  (cond((eq 'w32 window-system) "~/.emacs.d/")
        (t "~/.confg/emacs/")))


;; General Editor Settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default standard-indent 2)
(setq-default truncate-lines t)

;; Interface
(set-face-attribute 'default nil :family "RobotoMono Nerd Font" :height 160 :weight 'normal :width 'normal)
(set-fontset-font t 'symbol "Apple Color Emoji")
(pixel-scroll-precision-mode t)
(global-display-line-numbers-mode t)
(fido-vertical-mode t)
(fido-mode t)
(electric-pair-mode t)

;; Variables and Warnings
(setq inhibit-startup-message t)
(setq-default dired-kill-when-opening-new-dired-buffer t)
;; (setq auto-save-file-name-transforms `((".*" ,(concat (my/get-config-dir) "autosaves/") t)))
(setq backup-directory-alist `(("." . ,(concat (my/get-config-dir) "_backups/"))))
(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-deferred-compilation t)
(setq warning-minimum-level :error)
(setq ring-bell-function 'ignore)
(setq visible-bell t)
(setq tramp-default-method "sshx")

(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(p)" "BLOCKED(b)" "|" "DONE(d)" "CANCELLED(c)")))

;; OS Specific Settings
(when my/WSL
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "wslview"))

(unless my/TERM
  (global-set-key (kbd "C-z") 'undo)
  (global-set-key (kbd "C-S-z") 'undo-redo))

;; Custom Functions
(defvar my-clone-dir
  "Directory where repositories should be cloned."
  (cond ((eq 'w32 window-system) "D:/Code")
        (t "~/Workspace/github.com/eastwood/")))

(defun clone-repo (repo)
  "Clone each repository in `my-repos` into `my-clone-dir`."
  (interactive)
  (unless (file-directory-p my-clone-dir)
    (make-directory my-clone-dir t))
  (let ((repo-url (format "git@github.com:nib-group/%s.git" repo))
        (repo-path (expand-file-name repo my-clone-dir)))
    (unless (file-directory-p repo-path)
      (shell-command (format "git clone %s %s" repo-url repo-path)))))

(defun my/wsl-copy (start end)
  "Copy region to Windows clipboard."
  (interactive "r")
  (call-process-region start end "clip.exe" nil 0))

(defun my/wsl()
  "Return Windows clipboard as string."
  (let ((coding-system-for-read 'dos))
    (substring				; remove added trailing \n
     (shell-command-to-string
      "powershell.exe -Command Get-Clipboard") 0 -1)))

(defun my/wsl-paste (arg)
  "Insert Windows clipboard at point. With prefix ARG, also add to kill-ring"
  (interactive "P")
  (let ((clip (my/wsl)))
    (insert clip)
    (if arg (kill-new clip))))

(defun my/open-sales-zoom()
  (interactive)
  (let ((baseUrl "https://nibgroup.zoom.us/j/815911628?pwd=UnJBQ2hYaFBxOHBJazNzdzJ6TDc2UT09"))
    (browse-url baseUrl)))

(defun my/open-jira()
  (interactive)
  (let ((name (magit-get-current-branch)))
    (browse-url (concat "https://nibgroup.atlassian.net/browse/" name))))

(defun my/open-buildkite()
  "Open Buildkite in browser."
  (interactive)
  (let* ((project (project-current))
         (name (if project
                   (file-name-nondirectory (directory-file-name (project-root project)))
                 "default-project"))) ; fallback if no project is found
    (browse-url (concat "https://buildkite.com/nib-health-funds-ltd/" name))))

(defun my/kill-this-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun my/open-notes()
  (interactive)
  (find-file (concat org-directory "/inbox.org")))

(defun my/kill-magit-buffer ()
  (interactive)
  (magit-mode-bury-buffer 16))

(defun my/fetch (url &rest args)
  (interactive (browse-url-interactive-arg "URL: "))
  "Fetch URL and display in a new *response* buffer."
  (url-retrieve url
                (lambda (status)
                  (if (plist-get status :error)
                      (message "Error fetching offers: %S" (plist-get status :error))
                    (goto-char (point-min))
                    (search-forward "\n\n")
                    (let ((content (buffer-substring (point) (point-max)))
                          (buffer (get-buffer-create "*response*")))
                      (with-current-buffer buffer
                        (erase-buffer)
                        (insert content)
                        (goto-char (point-min))
                        (json-pretty-print-buffer)
                        (display-buffer buffer)))))))

(defun my/switch-to-project ()
  "Switch to a project and associate it with a perspective."
  (interactive)
  (let* ((project (project-prompt-project-dir))
         (project-name (file-name-nondirectory (directory-file-name project))))
    (if (member project-name (persp-all-names))
        (persp-switch project-name)
      (progn
        (persp-switch project-name)
        (project-switch-project project)))))

(defun my/code-directory(&optional suffix_path)
  (interactive)
  (let ((code-dir (cond ((eq 'w32 window-system) "D:/Code/")
                          (t "~/Workspace/github.com/eastwood/"))))
    (if suffix_path
        (concat code-dir suffix_path)
      code-dir)))


(defun my/open-config()
  (interactive)
  (find-file (concat (my/get-config-dir) "init.el")))

;; Package Configuration
(use-package which-key
  :config
  (which-key-mode))

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
  :unless (eq window-system 'w32)
  :config
  (exec-path-from-shell-initialize))

(use-package perspective
  :init
  (setq persp-suppress-no-prefix-key-warning t)
  (setq persp-initial-frame-name "main")
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
  :mode ("\\.ts\\'" "\\.js\\'" "\\.mjs\\'")
  :hook ((typescript-ts-mode . eglot-ensure))
  :hook ((tsx-ts-mode . eglot-ensure))
  :custom
  (typescript-ts-mode-indent-offset 2)
  (typescript-indent-level 2)
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))

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
  :hook (python-ts-mode . python-black-on-save-mode))

(use-package pyvenv
  :after python
  :hook ((python-ts-mode . pyvenv-tracking-mode)))

(use-package poetry
  :hook (python-ts-mode . poetry-tracking-mode))

(use-package multiple-cursors)

(use-package yaml-mode)

(use-package vterm
  :unless (eq window-system 'w32)
  :hook (vterm-mode . (lambda () (display-line-numbers-mode -1))))

(use-package doom-modeline)

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode))

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
  (setq org-directory (my/code-directory "notes"))
  :custom
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
  :init
  (add-to-list 'exec-path "/opt/homebrew/bin")
  :custom
  (copilot-indent-offset-warning-disable t)
  :config
  (add-to-list 'exec-path "/opt/homebrew/bin")
  (add-hook 'prog-mode-hook 'copilot-mode)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

(use-package gptel
  :vc (:url "https://github.com/karthink/gptel" :rev :newest)
  :init
  (setq auth-sources '("~/.authinfo"))
  (setq gptel-api-key (auth-source-pick-first-password :host "api.openai.com")))


;; Project configuration
(eval-after-load "dired"
  '(progn
     (put 'dired-find-alternate-file 'disabled nil)
     (define-key dired-mode-map (kbd "w") 'wdired-change-to-wdired-mode)))

(setq webjump-sites
      '(("Repository Search (nib)" .
         [simple-query "github.com"
                       "https://github.com/search?type=repositories&q=org%3Anib-group+"
                       #1=""])
        ("Story Search (nib)" .
         [simple-query "nibgroup.atlassian.net"
                       "https://nibgroup.atlassian.net/browse/"
                       #1=""])
        ))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration nil)
  (setq evil-undo-system 'undo-redo)
  (setq evil-want-C-u-scroll t)
  :config
  (setq evil-shift-width 2)
  (setq evil-normal-state-cursor '("green" box))
  (setq evil-insert-state-cursor '("red" bar))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-replace-state-cursor '("red" hollow))
  (setq evil-operator-state-cursor '("purple" hollow))
  (define-key evil-normal-state-map (kbd "C-d") 'evil-scroll-down)
  (define-key evil-normal-state-map (kbd "M-.") 'eglot-code-actions)
  (define-key evil-normal-state-map (kbd "C-.") 'eglot-code-actions)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(define-prefix-command 'my/buffer-map)
(define-prefix-command 'my/files-map)
(define-prefix-command 'my/eval-prefix-map)
(define-prefix-command 'my/editor-map)

(use-package evil-leader
  :after evil
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")

  (evil-leader/set-key
    "SPC" 'execute-extended-command
    "x" 'my/kill-this-buffer
    "b" 'my/buffer-map
    "f" 'my/files-map
    "g" 'magit-status
    "e" 'my/eval-prefix-map
    "." 'my/editor-map
    "p" project-prefix-map
  ))

;; File Bindings
(define-key 'my/files-map (kbd "s") #'save-buffer)
(define-key 'my/files-map (kbd "f") #'find-file)

;; Buffer Bindings
(define-key 'my/buffer-map (kbd "k") #'kill-buffer)
(define-key 'my/buffer-map (kbd "n") #'next-buffer)
(define-key 'my/buffer-map (kbd "p") #'previous-buffer)
(define-key 'my/buffer-map (kbd "b") #'switch-to-buffer)
(define-key 'my/buffer-map (kbd "d") #'dired)

;; Eval bindings
(define-key 'my/eval-prefix-map (kbd "e") #'eval-last-sexp)
(define-key 'my/eval-prefix-map (kbd "b") #'eval-buffer)
(define-key 'my/eval-prefix-map (kbd "r") #'eval-region)

;; Project bindings
(define-key project-prefix-map (kbd "J") #'my/open-jira)
(define-key project-prefix-map (kbd "Z") #'my/open-sales-zoom)
(define-key project-prefix-map (kbd "B") #'my/open-buildkite)
(define-key project-prefix-map (kbd ".") #'my/switch-to-project)

;; Org mode bindings
(define-key org-mode-map (kbd "C-c c") #'org-toggle-checkbox)
(evil-leader/set-key-for-mode 'org-mode "t" #'org-toggle-checkbox)
(evil-leader/set-key-for-mode 'org-mode "r" #'org-refile)

;; Global Bindings
(global-set-key (kbd "C-`") #'vterm)
(global-set-key (kbd "C-S-c") #'my/wsl-copy)
(global-set-key (kbd "C-S-v") #'my/wsl-paste)
(global-set-key (kbd "C-.") #'eglot-code-actions)
(global-set-key (kbd "M-.") #'eglot-code-actions)
(global-set-key (kbd "M-<up>") #'backward-paragraph)
(global-set-key (kbd "M-<down>") #'forward-paragraph)
(global-set-key (kbd "C-=") #'er/expand-region)
(global-set-key (kbd "C-j") #'join-line)
(global-set-key (kbd "M-p") #'flymake-goto-prev-error)
(global-set-key (kbd "M-n") #'flymake-goto-next-error)

;; Editor bindings
(define-key my/editor-map (kbd "r") #'eglot-rename)
(define-key my/editor-map (kbd "g") #'gptel-menu)
(define-key my/editor-map (kbd "*") #'mc/mark-all-dwim)
(define-key my/editor-map (kbd "l") #'mc/edit-beginnings-of-lines)
(define-key my/editor-map (kbd "c") #'my/open-config)
(define-key my/editor-map (kbd "n") #'my/open-notes)
(define-key my/editor-map (kbd ".") #'persp-switch)

;; Load custom file
(setq custom-file (concat (my/get-config-dir) "custom.el"))
(load custom-file)

(setq-default xref-search-program 'ripgrep)
