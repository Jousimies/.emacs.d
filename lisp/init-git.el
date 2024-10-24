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


(use-package transient
  :ensure nil
  :custom
  (transient-show-popup 0.5)
  (transient-history-file (expand-file-name "transient/history.el" cache-directory))
  (transient-levels-file (expand-file-name "transient/levels.el" cache-directory))
  (transient-values-file (expand-file-name "transient/values.el" cache-directory)))

(with-eval-after-load 'transient
  (transient-bind-q-to-quit))

(use-package git-timemachine
  :bind ("M-g t" . git-timemachine))

(use-package browse-at-remote
  :bind ("M-g b" . browse-at-remote))

;; (use-package blamer
;;   :load-path "~/.emacs.d/packages/blamer.el/"
;;   :hook (prog-mode . blamer-mode)
;;   :custom
;;   (blamer-idle-time 0.3)
;;   (blamer-min-offset 70)
;;   :custom-face
;;   (blamer-face ((t :foreground "#7a88cf"
;;                    :background unspecified
;;                    :height 140
;;                    :italic t))))

(provide 'init-git)
;;; init-git.el ends here.
