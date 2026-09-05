;; -*- lexical-binding: t; -*-

;; browse-at-remote
(global-set-key (kbd "M-g b") #'browse-at-remote)

;; magit
(global-set-key (kbd "C-x g") #'magit)
(with-eval-after-load 'magit
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-module-sections-hook 'magit-insert-modules-overview)
  (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpulled-from-pushremote)
  (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpushed-to-pushremote)
  ;; (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpushed-to-upstream)
  )


(provide 'init-git)
