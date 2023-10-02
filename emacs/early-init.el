(setq gc-cons-threshold (* 100 1000 1000))
(setq read-process-output-max (* 1024 1024))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Fullscreen window
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))
