(use-package emms
  :bind (("<f5>" . emms)
         ("C-<f5>" . my/toggle-emms))
  :config
  (setq emms-directory (expand-file-name "cache/emms" user-emacs-directory))
  (setq emms-player-list '(emms-player-mpv))
  (setq emms-browser-covers #'emms-browser-cache-thumbnail-async)
  (setq emms-playlist-buffer-name "*Music*")
  (add-to-list 'display-buffer-alist '((or (derived-mode . emms-playlist-mode)
                                           (derived-mode . emms-browser-mode))
                                       (display-buffer-in-tab)
                                       (tab-name "Media") (tab-group "Media")))

  (defun my/toggle-emms (arg)
    "Toggle Emms playback: start if stopped, stop if playing.
With universal argument (C-u), pause playback instead of stopping."
    (interactive "P")
    (if emms-player-playing-p
        (if arg
            (emms-stop)
          (emms-pause))
      (emms-start))))

(use-package emms-playlist-mode
  :after emms
  :config
  (setq emms-playlist-mode-center-when-go t))

(use-package emms-source-file
  :after emms
  :config
  (setq emms-source-playlist-default-format 'native)
  (setq emms-source-playlist-ask-before-overwrite nil)

  (setq emms-source-file-default-directory (expand-file-name "~/Music/")))

(use-package emms-history
  :hook ((emms-browser-show-display . emms-history-load)
         (emms-browser-hide-display . emms-history-save)))

(use-package emms-setup
  :after emms
  :config
  (emms-all))

(use-package emms-browser
  :after emms
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
  (setq lyrics-fetcher-genius-access-token
        (auth-source-pick-first-password :host "genius.com" :user "ID"))
  (lyrics-fetcher-use-backend 'neteasecloud))

(use-package consult-emms
  :bind ("M-<f5>" . consult-emms-library))

(provide 'init-emms)
;;; init-emms.el ends here.
