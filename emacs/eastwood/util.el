(provide 'util)

(defun open-config()
  (interactive)
  (find-file "~/.config/emacs/init.el"))

(require 'no-littering)
