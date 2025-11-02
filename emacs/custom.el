;; -*- lexical-binding: t; -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(modus-vivendi-tinted))
 '(custom-safe-themes
   '("5a4cdc4365122d1a17a7ad93b6e3370ffe95db87ed17a38a94713f6ffe0d8ceb"
     "2d74de1cc32d00b20b347f2d0037b945a4158004f99877630afc034a674e3ab7"
     "f9d423fcd4581f368b08c720f04d206ee80b37bfb314fa37e279f554b6f415e9"
     default))
 '(evil-undo-system 'undo-redo)
 '(ignored-local-variable-values '((evil-shift-width . 2)))
 '(org-safe-remote-resources '("\\`https://system2\\.io/assets/org/theme\\.setup\\'"))
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((claude-code-ide :url
                      "https://github.com/manzaltu/claude-code-ide.el")
     (agent-shell :url "https://github.com/xenodium/agent-shell")
     (acp :url "https://github.com/xenodium/acp.el")
     (opencode :url "https://github.com/colobas/opencode.el" :branch
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
 )
