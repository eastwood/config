(provide 'languages)

(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rs?\\'" . rust-ts-mode))

(rc/require 'markdown-mode)
