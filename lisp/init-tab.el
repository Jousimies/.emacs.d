(use-package tab-bar
  :hook (after-init . tab-bar-mode)
  :config
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-tab-hints nil)
  (setq tab-bar-show nil))

(use-package tabspaces
  :hook (tab-bar-mode . tabspaces-mode)
  :config
  (setq tabspaces-session-file
        (expand-file-name "cache/tabsession.el" user-emacs-directory))
  (setq tabspaces-use-filtered-buffers-as-default t))

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
    "gs" 'tab-switch)
  (evil-define-key 'motion org-agenda-mode-map
    "gs" 'tab-switch))

(provide 'init-tab)
;;; init-tab.el ends here.
