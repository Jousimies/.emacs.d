(use-package savehist
  :config
  (setq savehist-file (expand-file-name "cache/history" user-emacs-directory))
  (setq history-length 1000
        savehist-save-minibuffer-history 1
        savehist-additional-variables '(kill-ring
                                        search-ring
                                        regexp-search-ring)
        history-delete-duplicates t)
  (savehist-mode))

(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "cache/places" user-emacs-directory))
  (save-place-mode))

(use-package so-long
  :hook (text-mode . global-so-long-mode))

(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'org-mode-hook 'electric-pair-mode)

(use-package hippie-exp
  :bind ([remap dabbrev-expand] . hippie-expand)
  :config
  (setq hippie-expand-try-functions-list '(try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-all-abbrevs
                                           try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill
                                           try-complete-lisp-symbol-partially
                                           try-complete-lisp-symbol)))

(use-package pixel-scroll
  :config
  (pixel-scroll-precision-mode))

(use-package word-wrap-mode
  :hook (org-mode . word-wrap-whitespace-mode))

(use-package abbrev
  :hook ((org-mode . abbrev-mode)
         (LaTeX-mode . abbrev-mode)))

(use-package auto-save
  :demand t
  :config
  (setq auto-save-silent t)
  (setq auto-save-delete-trailing-whitespace t)
  (auto-save-enable))

(use-package expand-region
  :commands er/expand-region)

(with-eval-after-load 'evil
    (evil-define-key 'visual 'evil-visual-state-map
      "v" 'er/expand-region))

(with-eval-after-load 'outline
  (define-key outline-minor-mode-map (kbd "C-<tab>") 'bicycle-cycle)
  (define-key outline-minor-mode-map (kbd "S-<tab>") 'bicycle-cycle-global))

(use-package undo-fu)

(use-package undo-fu-session
  :after undo-fu
  :config
  (setq undo-fu-session-directory (expand-file-name "cache/undo-fu-session" user-emacs-directory))

  (defun z/undo-fu-session--make-file-name (filename)
         "Take the path FILENAME and return a name base on this. IDEA: subed.el"
         (expand-file-name
          (format
           "undo-fu-session_%s"
           (md5 (convert-standard-filename (expand-file-name filename))))
          undo-fu-session-directory))

  (advice-add 'undo-fu-session--make-file-name :override #'z/undo-fu-session--make-file-name)

  (undo-fu-session-global-mode))

(use-package vundo
  :bind ("C-x u" . vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.Trash")

(use-package hungry-delete
  :custom
  (hungry-delete-chars-to-skip " \t\n\r\f\v")
  :hook ((text-mode . hungry-delete-mode)
         (prog-mode . hungry-delete-mode)
         (org-mode . hungry-delete-mode)))

(use-package whitespace-cleanup-mode
  :config
  (whitespace-cleanup-mode))

(provide 'init-edit)
;;; init-edit.el ends here.
