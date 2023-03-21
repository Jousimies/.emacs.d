(use-package tab-bar
  :defer t
  :config
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-tab-hints nil)
  (setq tab-bar-show nil)
  (tab-bar-mode))

(use-package tabspaces
  :after tab-bar
  :config
  (setq tabspaces-session-file
        (expand-file-name "cache/tabsession.el" user-emacs-directory))
  (setq tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-mode))

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
    "gs" 'tab-switch)
  (evil-define-key 'motion org-agenda-mode-map
    "gs" 'tab-switch))

(provide 'init-tab)
;;; init-tab.el ends here.
