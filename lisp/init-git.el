;;; init-git.el ---  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;; Server and restart-emacs
;; Magit, a better git.

(when (maybe-require-package 'magit)
 (require-package 'git-timemachine)

 (with-eval-after-load 'consult
   (require-package 'consult-git-log-grep)
   (with-eval-after-load 'consult-git-log-grep
     (setq consult-git-log-grep-open-function #'magit-show-commit))))

(require-package 'forge)

(provide 'init-git)
;;; init-git.el ends here
