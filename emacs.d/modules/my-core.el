(defconst my/WINDOWS (memq window-system '(w32)))
(defconst my/TERM (memq window-system '(nil)))
(defconst my/OSX (memq window-system '(ns mac)))
(defconst my/WSL (memq window-system '(x nil)))
(defconst my/GTK (memq window-system '(pgtk)))

(defconst my/CUSTOM-FILE-PATH "~/.emacs.d/custom.el")
(defconst my/CONFIG-FILE "~/.emacs.d/init.el")
(defconst my/ORG-PATH (cond (my/WSL "/mnt/c/users/clint/iCloudDrive/Documents/notes")
                            (my/GTK "/mnt/c/users/clint/iCloudDrive/Documents/notes")
                            (t "~/Documents/notes")))

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


(defmacro open-org-file (filename)
  "Macro for opening files in org directory with FILENAME filepath."
  `(lambda ()
     (interactive)
     (find-file (concat my/ORG-PATH "/" ,filename) nil)))


(provide 'my-core)
