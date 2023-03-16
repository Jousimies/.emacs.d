(use-package magit
  :commands (magit magit-status magit-submodule-add)
  :bind ("C-x g" . magit)
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-module-sections-hook 'magit-insert-modules-overview)
  (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpulled-from-pushremote)
  (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpushed-to-upstream)
  (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpushed-to-pushremote))

(use-package transient
  :defer t
  :config
  (setq transient-levels-file (expand-file-name "cache/transient/levels.el" user-emacs-directory))
  (setq transient-values-file (expand-file-name "cache/transient/values.el" user-emacs-directory))
  (setq transient-history-file (expand-file-name "cache/transient/history.el" user-emacs-directory)))

(use-package forge
  :after magit
  :config
  (setq forge-database-file (expand-file-name "cache/forge-database.sqlite" user-emacs-directory)))

(use-package browse-at-remote
  :commands browse-at-remote)

(my/space-leader-def
    "or" '(browse-at-remote :wk "Open remote"))

(provide 'init-git)
;;; init-git.el ends here.
