(setq package-enable-at-startup nil)
(setq package-native-compile t)
(setq package-check-signature nil) 

(menu-bar-mode -1)
(scroll-bar-mode -1)
(xterm-mouse-mode 1)
(tool-bar-mode -1)
(modify-all-frames-parameters '((vertical-scroll-bars . nil)))

(eval-when-compile
  (require 'package)
  (unless (assoc-default "elpa" package-archives)
    (add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t))
  (unless (assoc-default "melpa" package-archives)
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
  (unless (assoc-default "org" package-archives)
    (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (require 'use-package)
  (setq use-package-verbose t
        use-package-always-defer t
        use-package-always-ensure t))

