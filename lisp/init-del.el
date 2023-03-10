(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.Trash")

(use-package hungry-delete
  :custom
  (hungry-delete-chars-to-skip " \t\n\r\f\v")
  :hook ((text-mode . hungry-delete-mode)
         (prog-mode . hungry-delete-mode)
         (org-mode . hungry-delete-mode)))

(use-package whitespace-cleanup-mode
  :hook (on-first-file . whitespace-cleanup-mode))

(provide 'init-del)
;;; init-del.el ends here.
