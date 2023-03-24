(use-package emms
  :bind ("<f5>" . emms)
  :config
  (setq emms-directory (expand-file-name "cache/emms" user-emacs-directory))
  (setq emms-player-list '(emms-player-mpv))
  (setq emms-browser-covers #'emms-browser-cache-thumbnail-async)
  (setq emms-playlist-buffer-name "*Music*")
  (add-to-list 'display-buffer-alist '((or (derived-mode . emms-playlist-mode)
                                           (derived-mode . emms-browser-mode))
                                       (display-buffer-in-tab)
                                       (tab-name "Media") (tab-group "Media"))))

(use-package emms-playlist-mode
  :after emms
  :config
  (setq emms-playlist-mode-center-when-go t))

(use-package emms-source-file
  :after emms
  :config
  (setq emms-source-file-default-directory "~/Music/"))

(use-package emms-setup
  :after emms
  :config
  (emms-all))

(use-package emms-browser
  :bind ("C-<f5>" . emms-smart-browse)
  :config
  (setq emms-browser-thumbnail-small-size 64)
  (setq emms-browser-thumbnail-medium-size 128)

  (evil-define-key 'normal emms-browser-mode-map
    "gb" nil))

(use-package emms-lyrics
  :hook (emms-player-started-hook . emms-lyrics-toggle-display-on-modeline))

(use-package lyrics-fetcher
:after (emms)
:config
(lyrics-fetcher-use-backend 'neteasecloud))

(use-package consult-emms
  :bind ("M-<f5>" . consult-emms-library))

(provide 'init-emms)
;;; init-emms.el ends here.
