;;;###autoload
(defun my/music ()
    (interactive)
    (emms-browser)
    (emms-playlist-mode-go))

;;;###autoload
(defun my/toggle-emms (arg)
    "Toggle Emms playback: start if stopped, stop if playing.
With universal argument (C-u), pause playback instead of stopping."
    (interactive "P")
    (if (get-buffer "*Music*")
        (if emms-player-playing-p
            (if arg
                (emms-stop)
              (emms-pause))
          (emms-start))
      (my/music)))

(use-package emms
  :bind (("<f8>" . my/toggle-emms)
         ("<f9>" . emms-seek-forward)
         ("<f7>" . emms-seek-backward))
  :config
  (setq emms-directory (expand-file-name "cache/emms" user-emacs-directory))
  (setq emms-player-list '(emms-player-mpv))
  (setq emms-browser-covers #'emms-browser-cache-thumbnail-async)
  (setq emms-playlist-buffer-name "*Music*")
  (add-to-list 'display-buffer-alist '((derived-mode . emms-playlist-mode)
                                       (display-buffer-in-side-window)
                                       (side . right)
                                       (window-parameters
                                        (window-width . 0.4)
                                        (select . t)
                                        (mode-line-format . none))))
  (evil-define-key 'normal emms-playlist-mode-map
    "q" 'quit-window))

(use-package emms-playlist-mode
  :commands emms-playlist-mode-go
  :general (my/space-leader-def
             "bp" '(my/switch-to-music :wk "Music"))
  :config
  (defun my/switch-to-music ()
    (interactive)
    (if (get-buffer "*Music*")
        (emms-playlist-mode-go)
      (my/music)))
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
  :commands emms-browser
  :config
  (setq emms-browser-thumbnail-small-size 64)
  (setq emms-browser-thumbnail-medium-size 128)

  (evil-define-key 'normal emms-browser-mode-map
    "gb" nil
    "q" 'quit-window))

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

(provide 'init-music)
;;; init-music.el ends here.
