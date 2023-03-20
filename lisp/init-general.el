(use-package general
  :config
  (general-create-definer my/space-leader-def
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
    :states '(normal visual insert emacs)))

(defun my/emacs-config ()
  "My literate Emacs configuration."
  (interactive)
  (find-file (expand-file-name "emacs_evil.org" user-emacs-directory)))

(my/space-leader-def
  ".i" '(my/emacs-config :wk "Configuration"))

(provide 'init-general)
;;; init-general.el ends here.
