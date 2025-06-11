;; Notes
;; Windows will need to install ripgrep + xargs ie:
;; winget.exe install GnuWin32.FindUtils
;; winget.exe install ripgrep
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

(defun my/get-config-dir()
  (cond((eq 'w32 window-system) "~/.emacs.d/")
        (t "~/.config/emacs/")))

;; General Editor Settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default standard-indent 2)
(setq-default truncate-lines t)

;; Interface
(set-face-attribute 'default nil :family "RobotoMono Nerd Font" :height 140 :weight 'normal :width 'normal)
(set-fontset-font t 'symbol "Apple Color Emoji")
(pixel-scroll-precision-mode t)
(global-display-line-numbers-mode t)
(fido-vertical-mode t)
(fido-mode t)
(electric-pair-mode t)

;; Variables and Warnings
(setq inhibit-startup-message t)
(setq-default dired-kill-when-opening-new-dired-buffer t)
(setq dired-dwim-target t)
;; (setq auto-save-file-name-transforms `((".*" ,(concat (my/get-config-dir) "autosaves/") t)))
(setq backup-directory-alist `(("." . ,(concat (my/get-config-dir) "_backups/"))))
(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-deferred-compilation t)
(setq warning-minimum-level :error)
(setq ring-bell-function 'ignore)
(setq visible-bell t)
(setq tramp-default-method "sshx")

(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(i)" "BLOCKED(b)" "|" "DONE(d)" "CANCELLED(c)")))

;; OS Specific Settings
(when my/WSL
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "wslview"))

(when my/TERM
  (use-package evil-terminal-cursor-changer
    :config
    (evil-terminal-cursor-changer-activate)))

(unless my/TERM
  (global-set-key (kbd "C-z") 'undo)
  (global-set-key (kbd "C-S-z") 'undo-redo))

;; Custom Functions
(defvar my/clone-dir
  (cond ((eq 'w32 window-system) "D:/Code")
        (t "~/Workspace/github.com/eastwood/")))

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

(defun my/notes-file()
  (concat org-directory "/inbox.org"))

(defun my/open-notes()
  (interactive)
  (find-file (my/notes-file)))

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

(use-package expand-region
  :bind ("C-=" . er/expand-region))

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
  (setq persp-suppress-no-prefix-key-warning t)
  (setq persp-initial-frame-name "main"))

(use-package move-text
  :commands (move-text-up move-text-down)
  :bind (("C-<up>" . move-text-up)
         ("C-<down>" . move-text-down)))

;; Don't need this with evil
;; (use-package expand-region
;;   :commands (er/expand-region))

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
  (setq js-indent-level 2)
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

(use-package inf-ruby
  :mode ("\\.rb\\'"))

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

(use-package yaml-mode)

(use-package vterm
  :bind (("C-`" . vterm))
  :unless (eq window-system 'w32)
  :hook (vterm-mode . (lambda () (display-line-numbers-mode -1))))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode))

(use-package terraform-mode
  :custom (terraform-indent-level 2)
  :config
  (defun my-terraform-mode-init ()
    (outline-minor-mode 1))
  (add-hook 'terraform-mode-hook 'my-terraform-mode-init))

(use-package editorconfig)

(use-package verb
  :config
  (setq verb-auto-kill-response-buffers 2)
  (setq verb-suppress-load-unsecure-prelude-warning t))

(defun my/export-archive ()
  "Export current Org heading subtree to UTF-8 text and append to archive/<date>.txt."
  (interactive)
  (let* ((date-str (format-time-string "%Y-%m-%d"))
         (archive-dir "archive/")
         (file (concat archive-dir date-str ".txt")))
    (save-excursion
      (org-back-to-heading t)
      (org-narrow-to-subtree)
      (let* ((exported (org-export-as 'ascii t nil t '(:ascii-charset utf-8)))
             (lines (split-string exported "\n"))
             ;; Remove the header/banner lines (assumes org-export-as ASCII output)
             (content (seq-drop-while (lambda (line)
                                        (or (string-match-p "\\`[ ━═]*\\'" line)
                                            (string-match-p "\\`Table of Contents\\'" line)))
                                      lines))
             ;; Remove "Exported at..." line if present
             (content (seq-remove (lambda (line)
                                    (string-match-p "\\`Exported at " line))
                                  content))
             ;; Build final text
             (final (concat "Archived: " date-str "\n\n"
                            (mapconcat #'identity content "\n"))))
        (unless (file-directory-p archive-dir)
          (make-directory archive-dir))
        (with-temp-buffer
          (insert final "\n\n")
          (if (file-exists-p file)
              (append-to-file (point-min) (point-max) file)
            (write-region (point-min) (point-max) file)))))
    (widen)
    (message "Archived to %s" file)))

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

(defun review-template()
"👋 **[[https://nibgroup.atlassian.net/browse/%^{Jira}][%\\1]]** is ready for review 🙏
**Pull Request:** %^{Pull Request}
**Buildkite:** %^{Buildkite}
Notes:
- %?
")

(use-package org
  :hook ((after-init . org-mode))
  :commands (org-agenda org-capture org-toggle-checkbox org-directory)
  :config
  (setq org-startup-indented t)
  (setq org-directory (my/code-directory "notes"))
  (setq org-agenda-files (list org-directory))
  (setq org-confirm-babel-evaluate nil)
  (setq org-export-with-section-numbers nil)
  (setq org-capture-templates
        `(("t" "Todo" entry (file+headline ,(my/notes-file) "Inbox") "* TODO %?\n  Created: %u")
          ("r" "Review Note" entry (file+headline ,(my/notes-file) "Review Notes")
           (function review-template) :empty-lines 1)))

  (define-key org-mode-map (kbd "C-c c") #'org-toggle-checkbox)
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  (evil-leader/set-key-for-mode 'org-mode "t" #'org-toggle-checkbox)
  (evil-leader/set-key-for-mode 'org-mode "r" #'org-refile)
  (evil-leader/set-key-for-mode 'org-mode ">" #'org-toggle-narrow-to-subtree)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ruby . t)
     (verb . t)
     (js . t)
     (shell . t))))

(use-package exec-path-from-shell
  :defer t
  :hook (after-init . exec-path-from-shell-initialize)
  :unless (eq window-system 'w32))

(use-package copilot
  :after exec-path-from-shell
  :vc (:url "https://github.com/copilot-emacs/copilot.el" :rev :newest :branch "main")
  :custom
  (copilot-indent-offset-warning-disable t)
  :config
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

(use-package gptel
  :config
  (gptel-make-gh-copilot "Copilot"))

(use-package aider
  :defer t
  :bind (("C-c a" . aider-transient-menu))
  :vc (:url "https://github.com/tninja/aider.el" :rev :newest :branch "main")
  :config
  (setq auth-sources '("~/.authinfo"))
  (setq aider-args '("--model" "o3-mini" "--no-auto-commits"))
  (let ((key (auth-source-pick-first-password
              :host "api.openai.com"
              :max 1)))
    (setenv "OPENAI_API_KEY" key)))

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

(setq evil-want-keybinding nil)
(setq evil-want-integration nil)

(use-package evil
  :custom
  (evil-undo-system 'undo-redo)
  :config
  (setq evil-want-C-u-scroll t)
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
  (define-key evil-normal-state-map (kbd "gd") 'xref-find-definitions)
  (define-key evil-normal-state-map (kbd "gD") 'xref-find-definitions-other-window)
  (define-key evil-normal-state-map (kbd "gr") 'xref-find-references)
  (define-key evil-normal-state-map (kbd "gD") 'xref-find-definitions-other-window)
  (define-key evil-normal-state-map (kbd "K") 'eglot-find-typeDefinition)
  (define-key evil-normal-state-map (kbd "=") 'eglot-format)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init '(magit dired xref messages)))

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
(define-key project-prefix-map (kbd "J") #'my/open-jira)
(define-key project-prefix-map (kbd "Z") #'my/open-sales-zoom)
(define-key project-prefix-map (kbd "B") #'my/open-buildkite)
(define-key project-prefix-map (kbd ".") #'my/switch-to-project)

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
(global-set-key (kbd "M-l") #'flymake-show-buffer-diagnostics)

;; Utility bindings
(define-key my/editor-map (kbd "r") #'eglot-rename)
(define-key my/editor-map (kbd "l") #'gptel-menu)
(define-key my/editor-map (kbd ".") #'persp-switch)

;; Org bindings
(define-key my/org-map (kbd "c") #'org-capture)

;; Load custom file
(setq custom-file (concat (my/get-config-dir) "custom.el"))
(load custom-file)
(setq-default xref-search-program 'ripgrep)
