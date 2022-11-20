;;; init-denote.el --- Denote. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
(when (maybe-require-package 'denote)
  (setq denote-directory (expand-file-name "denote" my-galaxy))
  (add-hook 'dired-mode-hook 'denote-dired-mode)

  (when (maybe-require-package 'consult-notes)
    (setq consult-notes-sources `(("Books" ?b ,(expand-file-name "books" denote-directory))
                                  ("Professional" ?p ,(expand-file-name "professional" denote-directory))))
    (when (locate-library "denote")
      (add-hook 'on-first-file-hook 'consult-notes-denote-mode)))

  (general-define-key
   :states '(normal visual emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "nN" '(denote-open-or-create :wk "Denote open or create")
   "nM" '(consult-notes :wk "Find notes")))

(provide 'init-denote)
;;; init-denote.el ends here
