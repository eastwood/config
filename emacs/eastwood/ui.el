(provide 'ui)

(column-number-mode 1)
(show-paren-mode 1)
(load-theme 'modus-vivendi)

(fido-vertical-mode)

(unless (display-graphic-p)
  (xterm-mouse-mode)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

