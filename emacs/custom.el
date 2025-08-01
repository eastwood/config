(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(nord))
 '(custom-safe-themes
   '("5a4cdc4365122d1a17a7ad93b6e3370ffe95db87ed17a38a94713f6ffe0d8ceb"
     default))
 '(evil-undo-system 'undo-redo)
 '(ignored-local-variable-values '((evil-shift-width . 2)))
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((aider :url "https://github.com/tninja/aider.el" :branch "main")
     (copilot :url "https://github.com/copilot-emacs/copilot.el"
              :branch "main")))
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
 )
(load-theme 'nord t)
