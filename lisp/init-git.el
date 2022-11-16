;;; init-git.el ---  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;; Server and restart-emacs
;; Magit, a better git.

(when (maybe-require-package 'magit)
  (require-package 'git-timemachine)

  (general-define-key
   :states 'normal
   :keymaps 'with-editor-mode-map
   "RET" "C-c C-c"))

(provide 'init-git)
;;; init-git.el ends here
