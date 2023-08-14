;; init-git.el --- Version manager. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package magit
  :bind ("C-x g" . magit)
  :config
  (setq magit-git-executable "/usr/bin/git")
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-module-sections-hook 'magit-insert-modules-overview)
  (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpulled-from-pushremote)
  (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpushed-to-upstream)
  (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpushed-to-pushremote))

(use-package git-timemachine
  :bind ("M-g t" . git-timemachine))

(use-package browse-at-remote
  :bind ("M-g b" . browse-at-remote))

(provide 'init-git)
;;; init-git.el ends here.
