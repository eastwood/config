(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(modus-vivendi))
 '(ignored-local-variable-values '((evil-shift-width . 2)))
 '(package-selected-packages
   '(ace-link ace-window aider copilot corfu-terminal doom-modeline ein
              evil-collection evil-leader evil-multiedit
              exec-path-from-shell expand-region git-link gptel
              inf-ruby move-text perspective poetry python-black
              python-mode terraform-mode treesit-auto verb vterm
              yaml-mode))
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
