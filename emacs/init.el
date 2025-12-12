;; Notes  -*- lexical-binding: t; -*-
(setq user-full-name "Clinton Ryan"
      user-mail-address "hello@clintonryan.com")

(defconst my/TERM   (eq window-system nil))
(defconst my/WSL     (and (eq system-type 'gnu/linux)
                          (getenv "WSLENV")))

(defconst my/IS-MAC (eq system-type 'darwin))

(require 'package)
(require 'use-package)

(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(add-hook 'emacs-startup-hook
          (lambda ()
            (require 'project)
	          (message "Emacs loaded in %s." (emacs-init-time))))

;; using the default git is dogshit slow on apple, use the brew one for now for 50% improvement
(setq my/paths-to-add
      (cond (my/IS-MAC (list "/Users/C.Ryan@nib.com.au/.nvm/versions/node/v22.15.0/bin" "/opt/homebrew/opt/curl/bin" "/opt/homebrew/bin" "/usr/local/bin" "/Users/C.Ryan@nib.com.au/.scripts" "/Users/C.Ryan@nib.com.au/go/bin" "/usr/local/go/bin" ))
            (my/WSL    (list "/home/eastwd/.nvm/versions/node/v22.18.0/bin" "/home/eastwd/go/bin"))))

;; Set paths for our packages
(let ((bins my/paths-to-add))
  (dolist (bin bins)
    (add-to-list 'exec-path bin)
    (setenv "PATH" (concat bin ":" (getenv "PATH")))))

(defun my/get-config-dir()
  (cond ((eq 'w32 window-system) "~/.emacs.d/")
        (t "~/.config/emacs/")))

;; General Editor Settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default standard-indent 2)

;; Interface
(set-face-attribute 'default nil :family "RobotoMono Nerd Font" :height (cond (my/IS-MAC 140) (t 120)) :weight 'normal :width 'normal)
(set-fontset-font t 'symbol "Apple Color Emoji")
(pixel-scroll-precision-mode t)
(global-display-line-numbers-mode t)
(fido-mode t)
(fido-vertical-mode t)
(electric-pair-mode t)
(global-auto-revert-mode t)

;; Variables and Warnings
(setq inhibit-startup-message t)
(setq-default dired-kill-when-opening-new-dired-buffer t)
(setq dired-dwim-target t)
;; (setq auto-save-file-name-transforms `((".*" ,(concat (my/get-config-dir) "autosaves/") t)))
(setq backup-directory-alist `(("." . ,(concat (my/get-config-dir) "_backups/"))))
(setq ring-bell-function 'ignore)
(setq visible-bell t)
(setq tramp-default-method "sshx")

;; OS Specific Settings
(when my/WSL
  (setenv "BROWSER" "explorer.exe")
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "wslview"))

(when my/TERM
  (use-package evil-terminal-cursor-changer
    :config
    (evil-terminal-cursor-changer-activate)))

;; Custom Functions
(defvar my/clone-dir
  (cond ((eq 'w32 window-system) "D:/Code")
        (t "~/Workspace/github.com/eastwood/")))

(defun my/notes-file()
  (concat org-directory "/inbox.org"))

(defun my/downloads-folder()
  "Return the path to the Downloads folder."
  (cond (my/WSL "/mnt/c/Users/clint/Downloads")
        (t "~/Downloads/")))

(defun my/open-downloads ()
  (interactive)
  (dired default-directory)
  (split-window-right)
  (other-window 1)
  (dired (my/downloads-folder)))

(defun my/rqp-auth (stage)
  "Authenticate RQP using SSO with the provided STAGE."
  (interactive (list (read-string "Stage: " "kaos")))
  (setenv "BROWSER" "xdg-open")
  (let ((cmd (format "rqp auth --sso -r poweruser -z secure -s %s" stage)))
    (async-shell-command cmd "*RQP Auth*")))

(defun my/clone-repo (repo &rest args)
  "Clone each repository in `my-repos` into `my-clone-dir`."
  (interactive (list (read-string "repo: ")))
  (unless (file-directory-p my/clone-dir)
    (make-directory my-clone-dir t))
  (let ((repo-url (format "git@github.com:nib-group/%s.git" repo))
        (repo-path (expand-file-name repo my/clone-dir)))
    (message "Cloning %s into %s" repo-url repo-path)
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

(defun my/set-interactive-param (name default-value)
  "Set an interactive parameter NAME with a DEFAULT-VALUE."
  (read-string (concat name " [" default-value "]: ") nil nil default-value))

(defun my/open-jira()
  "Open Jira issue for the given NAME or current branch if not provided."
  (interactive)
  (let* ((branch (my/set-interactive-param "Jira" (magit-get-current-branch))))
    (browse-url (concat "https://nibgroup.atlassian.net/browse/" branch))))

(defun my/open-buildkite ()
  "Open Buildkite in browser."
  (interactive)
  (let* ((default
           (ignore-errors
             (file-name-nondirectory
              (directory-file-name
               (project-root (project-current))))))
         (project (my/set-interactive-param "Project" (or default ""))))
    (browse-url
     (format "https://buildkite.com/nib-health-funds-ltd/%s" project))))

(defun my/kill-this-buffer ()
  (interactive)
  (kill-buffer-and-window))

(defun my/open-notes()
  (interactive)
  (find-file (my/notes-file)))

(defun my/kill-magit-buffer ()
  (interactive)
  (magit-mode-bury-buffer 16))

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

(defun my/code-directory (&optional suffix_path)
  (interactive)
  (let ((code-dir (cond ((eq 'w32 window-system) "D:/Code/")
                          (t "~/Workspace/github.com/eastwood/"))))
    (if suffix_path
        (concat code-dir suffix_path)
      code-dir)))

(defun my/open-config()
  (interactive)
  (find-file (concat (my/get-config-dir) "init.el")))

(use-package eldoc-box
  :config
  (eldoc-mode nil))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

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
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-auto t)
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil))

(use-package corfu-terminal
  :config
  (corfu-terminal-mode))

(use-package perspective
  :hook (after-init . persp-mode)
  :init
  (setq persp-modestring-short t)
  (setq persp-suppress-no-prefix-key-warning t)
  (setq persp-initial-frame-name "main"))

(use-package move-text
  :commands (move-text-up move-text-down)
  :bind (("M-<up>" . move-text-up)
         ("M-<down>" . move-text-down)))

(use-package ace-window
  :bind (("M-o" . ace-window)))

(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'" . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode))
  :hook ((typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure))
  :custom
  (typescript-ts-mode-indent-offset 2)
  (typescript-indent-level 2)
  :config
  (setq js-indent-level 2))

(use-package yaml-ts-mode
  :mode (("\\.yaml\\'" . yaml-ts-mode)
         ("\\.yml\\'" . yaml-ts-mode)))

(use-package json-ts-mode)
(use-package go-ts-mode
  :hook ((go-ts-mode . eglot-ensure))
  :custom
  (go-ts-mode-indent-offset 2))

(use-package ruby-ts-mode
  :hook ((ruby-ts-mode . eglot-ensure)))

;; (use-package inf-ruby
;;   :mode ("\\.rb\\'"))

(use-package python-mode
  :hook ((python-ts-mode . eglot-ensure))
  :config
  (setq python-indent-offset 4))

;; C# and .NET Configuration
(use-package csharp-mode
  :mode (("\\.cs\\'" . csharp-ts-mode)
         ("\\.csx\\'" . csharp-ts-mode))
  :hook ((csharp-ts-mode . eglot-ensure))
  :config
  (add-hook 'csharp-ts-mode-hook
            (lambda ()
              (setq-local eglot-inlay-hints-mode -1)
              (setq-local csharp-ts-mode-indent-offset 4)
              (setq-local tab-width 4)))

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(csharp-ts-mode . ("csharp-ls")))))

(defun my/open-vterm-at-project-root ()
  "Open vterm at the `project-current' root directory."
  (interactive)
  (let ((root (if (fboundp 'project-current)
                  (cdr (project-current))
                default-directory)))
    (when root
      (let ((default-directory (car (cdr root))))
        (vterm)))))

(use-package vterm
  :bind (("C-`" . 'my/open-vterm-at-project-root))
  :unless (eq window-system 'w32)
  :hook (vterm-mode . (lambda ()
                        (display-line-numbers-mode -1)
                        (turn-off-evil-mode))))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode))

(use-package editorconfig)

(use-package verb
  :commands (verb-command-map verb-fetch)
  :config
  (setq verb-trim-body-end "[ \t\n\r]+")
  (setq verb-auto-kill-response-buffers t)
  (setq verb-suppress-load-unsecure-prelude-warning t))

(defun my/retrieve-id (url)
  "Fetch JSON from join API by ID and pretty-print the response body in *Join* buffer."
  (with-current-buffer (url-retrieve-synchronously url t t)
    (goto-char url-http-end-of-headers)
    (let ((response (string-trim (buffer-substring-no-properties (point) (point-max)))))
      (with-current-buffer (get-buffer-create "*Join Response*")
        (erase-buffer)
        (insert response)
        (json-pretty-print-buffer)
        (display-buffer (current-buffer))
        (json-ts-mode)))))

(defun my/retrieve-join-id (id is-prod)
  "Retrieve Join API data by ID and display in *Join Response* buffer."
  (interactive
   (list (read-string "ID: ")
         (y-or-n-p "Is this a production? ")))
  (let* ((environment (if is-prod "prod" "kaos"))
         (url (format "https://join-api.%s.internal.nibit.com.au/retrieve?id=%s" environment id)))
    (my/retrieve-id url)))

(defun my/retrieve-session-id (id is-prod)
  (interactive
   (list (read-string "ID: ")
         (y-or-n-p "Is this a production? ")))
  (let* ((environment (if is-prod "prod" "kaos"))
         (url (format "https://session-api.%s.internal.nibit.com.au/v1/session/%s" environment id)))
    (my/retrieve-id url)))

(defun my/create-review-notes ()
  "Capture Build Info and save it to the clipboard."
  (interactive)
  (let* ((jira (read-string "Jira ID (PHISL-1000): "))
         (pull-request (read-string "Pull Request (URL): "))
         (buildkite (read-string "Buildkite (URL): "))
         (notes (read-string "Notes (multi-line, separate with ;): "))
         (formatted-notes (mapconcat (lambda (note) 
                                        (format "\t- %s" (string-trim note)))
                                      (split-string notes ";") "\n")))
    (let ((final-output
           (format "👋 **[[https://nibgroup.atlassian.net/browse/%s][%s]]** is ready for review 🙏\n\n**Pull Request:** %s\n\n**Buildkite:** %s\n\n**Notes:**\n%s"
                   jira jira pull-request buildkite formatted-notes))
          (buf (generate-new-buffer "*Review Notes*")))
      (message final-output)
      (with-current-buffer buf
        (org-mode)
        (insert final-output)
        (write-file "/tmp/review-notes.org")
        (org-html-export-to-html)
      (browse-url-of-file (expand-file-name "/tmp/review-notes.html")))
      (message "Captured saved to messages"))))

(setq org-directory (cond ((eq 'w32 window-system) "D:/Code/notes")
                               (t "~/Workspace/github.com/eastwood/notes")))
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :commands (org-agenda org-capture org-toggle-checkbox org-directory)
   :config
  (setq org-html-head "<link rel=\"stylesheet\" href=\"https://system2.io/assets/org/theme.css\">")
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "BLOCKED(b)" "|" "DONE(d)" "CANCELLED(c)")))
  (setq org-refile-targets '((nil :maxlevel . 1)
                             (org-agenda-files :maxlevel . 1)))
  (setq org-tag-alist '(("work" . ?w) ("personal" . ?p) ( "nz" . ?n)))
  (setq org-startup-indented nil)
  (setq org-agenda-files (list (concat org-directory "/inbox.org") (concat org-directory "/nib/nib-archive.org") (concat org-directory "/personal/personal-archive.org")))
  (setq org-log-done 'time)
  (setq org-agenda-custom-commands
        '(("y" "Closed yesterday"
           search (format "CLOSED: \[%s" (format-time-string "%Y-%m-%d" (time-subtract (current-time) (days-to-time 1)))))))
  (setq org-confirm-babel-evaluate nil)
  (setq org-export-with-section-numbers nil)
  (setq org-capture-templates
        `(("t" "Todo" entry (file+headline ,(my/notes-file) "Inbox") "* TODO %?\nCREATED: %u")
          ("m" "Meeting Notes" entry (file+headline ,(my/notes-file) "Notes ✏️") "* %^{Meeting}\n  SCHEDULED: %u\n  %?")))
  (define-key org-mode-map (kbd "C-c c") #'org-toggle-checkbox)
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  (evil-leader/set-key-for-mode 'org-mode "t" #'org-toggle-checkbox)
  (evil-leader/set-key-for-mode 'org-mode "r" #'org-refile)
  (evil-leader/set-key-for-mode 'org-mode ">" #'org-toggle-narrow-to-subtree)
  (org-babel-do-load-languages 'org-babel-load-languages '((ruby . t) (verb . t) (js . t) (shell . t))))

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el" :rev :newest :branch "main")
  :config
  (setq copilot-indent-offset-warning-disable t)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu)
  :init
  (setenv "CLAUDE_CODE_USE_BEDROCK" "1")
  (setenv "AWS_REGION" "ap-southeast-2")
  (setenv "ANTHROPIC_MODEL" "arn:aws:bedrock:ap-southeast-2:384553929753:application-inference-profile/z9edt4wigsx1")
  (setenv "ANTHROPIC_SMALL_FAST_MODEL" "arn:aws:bedrock:ap-southeast-2:384553929753:application-inference-profile/sq9m6du6c693")
  :config
  (claude-code-ide-emacs-tools-setup))

(use-package gptel
  :commands (gptel-menu)
  :init
  (setq gptel-model 'claude-sonnet-4.5)
  (setq gptel-backend (gptel-make-gh-copilot "Copilot"))
  :config
  (require 'gptel-integrations)
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user:\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant:\n")
  (setq gptel-default-mode 'org-mode))

(use-package mcp
  :after gptel
  :config
  (setq mcp-hub-servers '(("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" "~/Workspace/github.com/eastwood")))))
  (require 'mcp-hub)
  ;; :custom (gptel-org-branching-context t)
  )

(defun my/set-aws-env-vars-from-file (filepath)
  "Extract AWS credentials from FILEPATH and set as environment variables.
Assumes credentials are in the [default] section."
  (interactive "fAWS credentials file: ")
  (with-temp-buffer
    (insert-file-contents filepath)
    (goto-char (point-min))
    (when (re-search-forward "^\\[default\\]" nil t)
      (let ((section-end (or (and (re-search-forward "^\\[" nil t)
                                  (match-beginning 0))
                             (point-max))))
        (save-excursion
          (goto-char (point-at-eol)) ; after [default]
          (when (re-search-forward "aws_access_key_id[ \t]*=[ \t]*\\([A-Z0-9]+\\)" section-end t)
            (setenv "AWS_ACCESS_KEY_ID" (match-string 1)))
          (goto-char (point-at-bol))
          (when (re-search-forward "aws_secret_access_key[ \t]*=[ \t]*\\([A-Za-z0-9+/=]+\\)" section-end t)
            (setenv "AWS_SECRET_ACCESS_KEY" (match-string 1)))
          (goto-char (point-at-bol))
          (when (re-search-forward "aws_session_token[ \t]*=[ \t]*\\([A-Za-z0-9+/=]+\\)" section-end t)
            (setenv "AWS_SESSION_TOKEN" (match-string 1)))
          (goto-char (point-at-bol))
          (when (re-search-forward "expiration[ \t]*=[ \t]*\\([0-9T:-]+Z\\)" section-end t)
            (setenv "AWS_EXPIRATION" (match-string 1))))))))

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
        ("Code Search (nib)" .
         [simple-query "github.com/nib-group"
                       "https://github.com/search?type=code&q=org%3Anib-group+"
                       #1=""])
        ("Story Search (nib)" .
         [simple-query "nibgroup.atlassian.net"
                       "https://nibgroup.atlassian.net/browse/"
                       #1=""])
        ))

(use-package evil
  :hook (after-init . evil-mode)
  :custom
  (evil-undo-system 'undo-redo)
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration nil)
  :config
  (setq evil-want-C-u-scroll nil)
  (setq evil-shift-width 2)
  (setq evil-normal-state-cursor '("green" box))
  (setq evil-insert-state-cursor '("red" bar))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-replace-state-cursor '("red" hollow))
  (setq evil-operator-state-cursor '("purple" hollow))
  (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
  (define-key evil-normal-state-map (kbd "C-d") 'evil-scroll-down)
  (define-key evil-normal-state-map (kbd "C-.") 'eglot-code-actions)
  (define-key evil-normal-state-map (kbd "M-.") 'eglot-code-actions)
  (define-key evil-normal-state-map (kbd "gi") 'eglot-find-implementation)
  (define-key evil-normal-state-map (kbd "gd") 'xref-find-definitions)
  (define-key evil-normal-state-map (kbd "gD") 'xref-find-definitions-other-window)
  (define-key evil-normal-state-map (kbd "gr") 'xref-find-references)
  (define-key evil-normal-state-map (kbd "K") 'eldoc-box-help-at-point)
  (define-key evil-normal-state-map (kbd "=") 'eglot-format)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up))
  
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init '(magit dired xref messages compile)))

(define-prefix-command 'my/buffer-map)
(define-prefix-command 'my/files-map)
(define-prefix-command 'my/eval-prefix-map)
(define-prefix-command 'my/editor-map)
(define-prefix-command 'my/org-map)

(use-package evil-leader
  :after evil
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")

  (evil-leader/set-key
    "SPC" 'execute-extended-command
    "x" 'my/kill-this-buffer
    "X" 'my/kill-this-buffer
    "b" 'my/buffer-map
    "f" 'my/files-map
    "g" 'magit-status
    "d" 'dired
    "e" 'my/eval-prefix-map
    "." 'my/editor-map
    "," 'persp-switch
    "o" 'my/org-map
    "p" project-prefix-map
    ))

;; File Bindings
(define-key 'my/files-map (kbd "s") #'save-buffer)
(define-key 'my/files-map (kbd "f") #'find-file)
(define-key 'my/files-map (kbd "c") 'my/open-config)
(define-key 'my/files-map (kbd "n") 'my/open-notes)

;; Buffer Bindings
(define-key 'my/buffer-map (kbd "k") #'kill-buffer)
(define-key 'my/buffer-map (kbd "n") #'next-buffer)
(define-key 'my/buffer-map (kbd "p") #'previous-buffer)
(define-key 'my/buffer-map (kbd "b") #'switch-to-buffer)

;; Eval bindings
(define-key 'my/eval-prefix-map (kbd "e") #'eval-last-sexp)
(define-key 'my/eval-prefix-map (kbd "b") #'eval-buffer)
(define-key 'my/eval-prefix-map (kbd "r") #'eval-region)

;; Project bindings
(define-key project-prefix-map (kbd ".") #'my/switch-to-project)

;; Global Bindings
(global-set-key (kbd "C-S-c") #'my/wsl-copy)
(global-set-key (kbd "C-S-v") #'my/wsl-paste)
(global-set-key (kbd "C-.") #'eglot-code-actions)
(global-set-key (kbd "M-.") #'eglot-code-actions)
(global-set-key (kbd "M-j") #'duplicate-line)
(global-set-key (kbd "M-p") #'flymake-goto-prev-error)
(global-set-key (kbd "M-n") #'flymake-goto-next-error)
(global-set-key (kbd "M-l") #'flymake-show-buffer-diagnostics)

;; Utility bindings
(define-key my/editor-map (kbd "c") #'compile)
(define-key my/editor-map (kbd "r") #'eglot-rename)
(define-key my/editor-map (kbd "l") #'gptel-menu)
(define-key my/editor-map (kbd ".") #'persp-switch)
(define-key my/editor-map (kbd "j") #'my/open-jira)
(define-key my/editor-map (kbd "z") #'my/open-sales-zoom)
(define-key my/editor-map (kbd "b") #'my/open-buildkite)
(define-key my/editor-map (kbd "g") #'git-link-dispatch)
(define-key my/editor-map (kbd "w") #'webjump)

;; Org bindings
(define-key my/org-map (kbd "a") #'org-agenda)
(define-key my/org-map (kbd "c") #'org-capture)
(define-key my/org-map (kbd "d") #'my/open-downloads)
(define-key my/org-map (kbd "i") #'org-toggle-inline-images)

(with-eval-after-load 'image-mode
  (define-key image-mode-map (kbd "C-c -") 'image-decrease-size)
  (define-key image-mode-map (kbd "C-c -") 'image-increase-size))

;; Load custom file
(setq custom-file (concat (my/get-config-dir) "custom.el"))
(load custom-file)
