(use-package evil
  :demand t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-set-undo-system 'undo-fu)
  :config
  (setq evil-want-C-u-scroll t)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init '(magit dired)))

(use-package evil-surround
  :hook (evil-mode . global-evil-surround-mode))

(use-package evil-leader
  :demand t
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
    "eb" 'eval-buffer
    "ee" 'eval-last-sexp
    "er" 'eval-region
    "fs" 'save-buffer
    "fow" (open-org-file "work.org")
    "foh" (open-org-file "personal.org")
    "fd" 'open-org-directory
    "ff" 'counsel-find-file
    "fr" 'counsel-recentf
    "fed" 'open-config-file
    "feR" 'reload-config-file
    "is" 'yas-insert-snippet
    "in" 'yas-new-snippet
    "gs" 'magit-status
    "pf" 'counsel-projectile-find-file
    "sg" 'counsel-projectile-rg
    "sw" 'my/search-current-word
    "sb" 'swiper
    "pr" 'counsel-register
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

(provide 'my-evil)
