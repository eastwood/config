;; -*- lexical-binding: t; -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0ed2cdf8812079071bb9693e4c3a013ced99d3b18ef2bd24ee1796f92977779f"
     "1781e8bccbd8869472c09b744899ff4174d23e4f7517b8a6c721100288311fa5"
     "e7820b899036ae7e966dcaaec29fd6b87aef253748b7de09e74fdc54407a7a02"
     "de8f2d8b64627535871495d6fe65b7d0070c4a1eb51550ce258cd240ff9394b0"
     "5a4cdc4365122d1a17a7ad93b6e3370ffe95db87ed17a38a94713f6ffe0d8ceb"
     default))
 '(evil-undo-system 'undo-redo)
 '(ignored-local-variable-values '((evil-shift-width . 2)))
 '(org-safe-remote-resources
   '("\\`https://system2\\.io/assets/org/theme\\.setup\\'"
     "\\`https://gitlab\\.com/OlMon/org-themes/-/raw/master/src/simple_inline/simple_inline\\.theme\\'"))
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((nano-theme :url "https://github.com/rougier/nano-theme" :branch
                 "main")))
 '(xref-search-program-alist
   '((grep . "xargs -0 -s 10000 grep <C> --null -snHE -e <R>")
     (ripgrep
      . "xargs -0 -s 10000 rg <C> --null -nH --no-heading --no-messages -e <R>")
     (ugrep . "xargs -0 -s 10000 ugrep <C> --null -ns -e <R>"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:background "#2a2e36" :foreground "#d8dee9"))))
 '(org-block-begin-line ((t (:foreground "#81a1c1" :background "#2a2e36" :weight bold))))
 '(org-block-end-line ((t (:foreground "#81a1c1" :background "#2a2e36" :weight bold))))
 '(org-quote ((t (:background "#2a2e36" :foreground "#d8dee9" :slant italic))))
 '(org-quote-begin-line ((t (:foreground "#81a1c1" :background "#2a2e36" :weight bold))))
 '(org-quote-end-line ((t (:foreground "#81a1c1" :background "#2a2e36" :weight bold)))))
