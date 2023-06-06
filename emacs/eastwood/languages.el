(provide 'languages)

(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-ts-mode))
(add-hook 'typescript-ts-mode 'eglot)

(rc/require 'markdown-mode)
