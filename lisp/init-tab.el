;; init-tab.el --- Tab. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package tab-bar
  :config
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-tab-hints nil)
  (setq tab-bar-show nil)
  (tab-bar-mode))

(use-package tabspaces
  :commands tabspaces-switch-to-buffer
  :hook (tab-bar . tabspaces-mode)
  :config
  (setq tabspaces-session-file
        (expand-file-name "cache/tabsession.el" user-emacs-directory))
  (setq tabspaces-include-buffers '())
  (setq tabspaces-use-filtered-buffers-as-default t))

(provide 'init-tab)
;;; init-tab.el ends here.
