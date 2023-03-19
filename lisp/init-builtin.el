(setq ring-bell-function 'ignore)
(setq tab-width 4)
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq use-short-answers t)
(setq read-process-output-max #x10000)
(setq create-lockfiles nil)
(setq recenter-redisplay nil)
(setq load-prefer-newer t)
(setq next-screen-context-lines 5)
(setq frame-inhibit-implied-resize t)
(setq inhibit-compacting-font-caches t)
(setq inhibit-quit nil)
(setq fast-but-imprecise-scrolling t)
(setq scroll-preserve-screen-position t)
(setq auto-save-list-file-name nil)
(setq history-delete-duplicates t)
(setq bidi-display-reordering nil)

(setq auto-save-list-file-prefix (expand-file-name "cache/auto-save-list/.saves-" user-emacs-directory))
(setq inhibit-default-init t)
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)

(use-package simple
  :defer t
  :init
  (setq-default indent-tabs-mode nil)
  :config
  (setq mark-ring-max 128)
  (setq kill-do-not-save-duplicates t)
  (setq kill-ring-max (* kill-ring-max 2))
  (setq async-shell-command-display-buffer nil))

(use-package files
  :defer t
  :config
  (setq auto-save-default nil)
  (setq large-file-warning-threshold nil)
  (setq confirm-kill-processes nil)
  (setq confirm-kill-emacs nil)
  (setq make-backup-files nil)
  (setq view-read-only t)
  (setq kill-read-only-ok t)

  (defun my/auto-create-missing-dirs ()
    (let ((target-dir (file-name-directory buffer-file-name)))
      (unless (file-exists-p target-dir)
        (make-directory target-dir t))))
  (add-to-list 'find-file-not-found-functions #'my/auto-create-missing-dirs))

(use-package ffap
  :defer t
  :config
  (setq ffap-machine-p-known 'reject))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; (my/space-leader-def
;;   "b" '(:ignore t :wk "Buffer/Bookmark")
;;   "be" '(eval-buffer :wk "Eval buffer")
;;   "bk" '(kill-this-buffer :wk "Kill This Buffer"))

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
    "gB" 'switch-to-prev-buffer
    "gb" 'switch-to-buffer
    "zx" 'kill-current-buffer))

(use-package calc
  :general (my/space-leader-def
             "C" '(calc :wk "calc"))
  :hook ((calc-trail-mode . (lambda ()
                              (setq-local mode-line-format nil)))
         (calc-mode . (lambda ()
                        (setq-local mode-line-format nil))))
  :config
  (setq calc-window-height 15))

(add-hook 'prog-mode-hook 'column-number-mode)

(add-hook 'find-file-hook 'size-indication-mode)

(add-hook 'on-first-input-hook 'delete-selection-mode)

(use-package autorevert
  :hook (on-first-file . global-auto-revert-mode))

(use-package menu-bar
  :general (my/space-leader-def
             "bk" '(kill-this-buffer :wk "Kill buffer")))

(use-package savehist
  :hook (on-first-file . savehist-mode)
  :config
  (setq savehist-file (expand-file-name "cache/history" user-emacs-directory))
  (setq history-length 1000
        savehist-save-minibuffer-history 1
        savehist-additional-variables '(kill-ring
                                        search-ring
                                        regexp-search-ring)
        history-delete-duplicates t))

(use-package saveplace
  :hook (on-first-file . save-place-mode)
  :config
  (setq save-place-file (expand-file-name "cache/places" user-emacs-directory)))
(add-hook 'on-first-buffer-hook 'save-place-mode)

(add-hook 'on-first-buffer-hook 'midnight-mode)

(use-package so-long
  :hook (text-mode . global-so-long-mode))

(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'org-mode-hook 'electric-pair-mode)

(use-package prog-mode
  :hook ((prog-mode . prettify-symbols-mode)
         (LaTeX-mode . prettify-symbols-mode))
  :config
  (setq prettify-symbols-alist '(("lambda" . ?λ)
                                 ("function" . ?𝑓))))

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

(add-hook 'prog-mode-hook 'outline-minor-mode)

(use-package pixel-scroll
  :hook (on-first-file . pixel-scroll-precision-mode))

(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
(add-hook 'org-roam-mode-hook 'turn-on-visual-line-mode)
(add-hook 'LaTeX-mode-hook #'turn-on-visual-line-mode)

(add-hook 'org-mode-hook 'word-wrap-whitespace-mode)
(add-hook 'org-roam-mode-hook 'word-wrap-whitespace-mode)

(use-package dired
  :bind ("C-x d" . dired)
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq insert-directory-program "/opt/homebrew/bin/gls")
  (setq dired-use-ls-dired t)
  (setq dired-dwim-target t)
  (setq dired-auto-revert-buffer #'dired-buffer-stale-p)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'top)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  (setq dired-auto-revert-buffer t)
  (add-to-list 'display-buffer-alist '((or (derived-mode . dired-mode)
                                           (derived-mode . dirvish-mode))
                                       (display-buffer-in-tab)
                                       (tab-name . "Dired")
                                       (tab-group . "Dired"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (:map dired-mode-map
              ("s-." . dired-hide-dotfiles-mode)))

(use-package image-dired
  :bind ("C-c d" . image-dired)
  :config
  (setq image-dired-dir (expand-file-name "cache/image-dired" user-emacs-directory)))

(use-package frame
  :hook (after-init . window-divider-mode)
  :config
  (face-spec-set 'window-divider
                 '((((background light))
                    :foreground "#000000")
                   (t
                    :foreground "#FFFFFF"))
                 'face-override-spec)
  (setq window-divider-default-bottom-width 1)
  (setq window-divider-default-places 'bottom-only))

(use-package doc-view
  :defer t
  :config
  (setq doc-view-mupdf-use-svg t)
  (setq doc-view-imenu-flatten t)
  (setq doc-view-continuous t))

(add-hook 'org-mode-hook 'abbrev-mode)
(add-hook 'LaTeX-mode-hook 'abbrev-mode)

(use-package bookmark
  :general (my/space-leader-def
             "ba" 'bookmark-set
             "br" 'bookmark-rename
             "bd" 'bookmark-delete
             "bj" 'bookmark-jump)
  :config
  (setq bookmark-default-file (expand-file-name "cache/bookmarks" user-emacs-directory)))

(use-package select
  :defer t
  :config
  (setq select-enable-primary t))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package url
  :defer t
  :config
  (setq url-configuration-directory (expand-file-name "cache/url" user-emacs-directory)))

(use-package multisession
  :defer t
  :config
  (setq multisession-directory (expand-file-name "cache/multisession" user-emacs-directory)))

(use-package server
  :hook (on-first-input . server-start)
  :general (my/space-leader-def
             "q" '(:ignore t :wk "Quit/Restart")
             "qR" '(restart-emacs :wk "Restart emacs")
             "qq" '(server-force-delete :wk "Server Delete")
             "qs" '(my/start-server :wk "Server Delete"))
  :config
  (defun my/start-server ()
    (interactive)
    (if (not (server-running-p))
        (server-start))
    (message "Server has started")))

(provide 'init-builtin)
;;; init-builtin.el ends here.
