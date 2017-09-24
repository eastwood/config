;;; package --- Summary
;;; Commentary:
;;; Sweet configuration file that uses babel for loading org based config
;;; Code:
;;; Dynamically bootstrap org-init.el file using org-babel-load-file

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(org-babel-load-file (expand-file-name "~/.emacs.d/org-init.org"))
(provide 'init)
;;; init.el ends here
(put 'dired-find-alternate-file 'disabled nil)
