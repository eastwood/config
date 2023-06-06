
;;; Code:

;; Optimisations
(setq gc-cons-threshold most-positive-fixnum)
(setq read-process-output-max (* 1024 1024))
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

(require 'package)

;; package
(setq package-enable-at-startup nil)
(setq package-native-compile t)
(setq package-check-signature nil)
(setq package-quickstart t)

;; compilations
(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-deferred-compilation t)

(setq inhibit-startup-message t)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(modify-all-frames-parameters '((vertical-scroll-bars . nil)))
(customize-set-variable 'initial-major-mode 'fundamental-mode)

(unless (display-graphic-p)
  (xterm-mouse-mode)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

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

