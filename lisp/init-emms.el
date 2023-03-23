(use-package emms
  :config
  (setq emms-directory (expand-file-name "cache/emms" user-emacs-directory))
  (setq emms-player-list '(emms-player-mpv))
  (setq emms-browser-covers #'emms-browser-cache-thumbnail-async)
  (setq emms-playlist-buffer-name "*Music*"))

(use-package emms-playlist-mode
  :config
  (setq emms-playlist-mode-center-when-go t))

(use-package emms-source-file
  :config
  (setq emms-source-file-default-directory "~/Music/"))

(use-package emms-setup
  :config
  (emms-all))

(use-package emms-browser
  :config
  (setq emms-browser-thumbnail-small-size 64)
  (setq emms-browser-thumbnail-medium-size 128)
  (evil-define-key 'normal emms-browser-mode-map
    "gb" nil))

(use-package emms-lyrics
  :after emms
  :config
  (emms-lyrics-toggle-display-on-modeline))

(use-package lyrics-fetcher
  :after (emms)
  :config
  (lyrics-fetcher-use-backend 'neteasecloud))

(provide 'init-emms)
;;; init-emms.el ends here.
