;; init.el --- auto-save *- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package server
  :hook (on-first-input . server-start)
  :config
  (defun my/start-server ()
    (interactive)
    (if (not (server-running-p))
        (server-start))
    (message "Server has started")))

(my/space-leader-def
  "q" '(:ignore t :wk "Quit/Restart")
  "qR" '(restart-emacs :wk "Restart emacs")
  "qq" '(server-force-delete :wk "Server Delete")
  "qs" '(my/start-server :wk "Server Delete"))

(provide 'init-server)
;;; init-server.el ends here.
