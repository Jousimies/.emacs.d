;; -*- lexical-binding: t; -*-

(when (and sys/macp (executable-find "gls"))
  (setq dired-use-ls-dired nil
        insert-directory-program "gls"
        dired-listing-switches
	"-l --almost-all --human-readable --group-directories-first --no-group"))

(with-eval-after-load 'dired
  (setq dired-dwim-target t
        dired-listing-switches "-alh --group-directories-first"
	dired-auto-revert-buffer #'dired-buffer-stale-p
        dired-kill-when-opening-new-dired-buffer t
        dired-recursive-copies 'always
        dired-recursive-deletes 'top
	dired-filename-display-length 'window))

(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'hl-line-mode)
(add-hook 'dired-mode-hook
          (lambda () (setq-local truncate-lines t)))

;; dired-do-shell-command, open file with default application.
(let ((cmd (cond ((and (eq system-type 'darwin) (display-graphic-p)) "open")
                 ((and (eq system-type 'gnu/linux) (display-graphic-p)) "xdg-open")
                 ((and (eq system-type 'windows-nt) (display-graphic-p)) "cmd /c start \"\"")
                 (t ""))))
  (setq dired-guess-shell-alist-user
        `(("\\.\\(?:docx\\|doc\\|xlsx\\|xls\\|ppt\\|pptx\\)\\'" ,cmd)
	  ("\\.\\(?:eps\\|dwg\\|psd\\|drawio\\)\\'" ,cmd)
          ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
          ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
          ("\\.\\(?:xcf\\)\\'" ,cmd)
	  ("\\.\\(?:epub\\|pdf\\)\\'" ,cmd)
          ("\\.csv\\'" ,cmd)
          ("\\.tex\\'" ,cmd)
          ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
          ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd))))


;; dired-omit-mode
(add-hook 'dired-mode-hook #'dired-omit-mode)
(setq dired-omit-verbose nil
      dired-omit-files "^\\.[^.].*")

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-'") 'my/org-attach-visit-headline-from-dired))

(add-hook 'dired-mode-hook #'diredfl-mode)

(keymap-global-set "C-c c i" #'file-info-show)

(keymap-global-set "C-x C-n" #'dired-sidebar-toggle-sidebar)

(provide 'init-dired)
