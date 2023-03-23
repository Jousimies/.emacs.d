(use-package tab-bar
  :config
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-tab-hints nil)
  (setq tab-bar-show nil)
  (tab-bar-mode))

(use-package tabspaces
  :config
  (setq tabspaces-session-file
        (expand-file-name "cache/tabsession.el" user-emacs-directory))
  (setq tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-mode))

(evil-define-key '(normal visual motion) 'global
    "gb" 'tabspaces-switch-to-buffer)

(evil-define-key '(normal visual motion) 'global
  "gs" 'tab-switch)

(provide 'init-tab)
;;; init-tab.el ends here.
