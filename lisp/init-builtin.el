;; init-builtin.el --- Builtin *- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(setq frame-inhibit-implied-resize t)
(setq use-file-dialog nil)
(setq use-dialog-box nil)

(setq-default ring-bell-function 'ignore
              use-short-answers t
              read-process-output-max #x10000
              message-kill-buffer-on-exit t
              message-kill-buffer-query nil
              indent-tabs-mode nil
              tab-width 4
              make-backup-files nil
              create-lockfiles nil
              confirm-kill-processes nil
              confirm-kill-emacs nil
              recenter-redisplay nil
              load-prefer-newer t
              mark-ring-max 128
              next-screen-context-lines 5
              inhibit-default-init t
              inhibit-startup-message t
              inhibit-splash-screen t
              inhibit-compacting-font-caches t
              ;; inhibit-quit nil
              fast-but-imprecise-scrolling t
              scroll-preserve-screen-position t
              auto-save-default nil
              auto-save-list-file-name nil
              kill-do-not-save-duplicates t
              kill-ring-max (* kill-ring-max 2)
              history-delete-duplicates t
              view-read-only t
              kill-read-only-ok t
              async-shell-command-display-buffer nil
              ;; Improve the performance of rendering long lines.
              bidi-display-reordering nil)

(setq ffap-machine-p-known 'reject)
(setq auto-save-list-file-prefix (expand-file-name "cache/auto-save-list/.saves-" user-emacs-directory))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(add-hook 'profiler-report-mode-hook #'hl-line-mode)

(setq switch-to-buffer-in-dedicated-window 'pop)
(setq switch-to-buffer-obey-display-actions t)

(my/space-leader-def
  "b" '(:ignore t :wk "Buffer/Bookmark")
  "be" '(eval-buffer :wk "Eval buffer")
  "bk" '(kill-this-buffer :wk "Kill This Buffer"))

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
    "gB" 'switch-to-prev-buffer
    "gb" 'switch-to-buffer
    "zx" 'kill-current-buffer))

(use-package calc
  :hook ((calc-trail-mode . (lambda ()
                              (setq-local mode-line-format nil)))
         (calc-mode . (lambda ()
                        (setq-local mode-line-format nil))))
  :config
  (setq calc-window-height 15))

(my/space-leader-def
  "C" '(calc :wk "calc"))

(add-hook 'prog-mode-hook 'column-number-mode)

(add-hook 'find-file-hook 'size-indication-mode)

(add-hook 'on-first-input-hook 'delete-selection-mode)

(setq-default winner-dont-bind-my-keys t)
(add-hook 'on-first-buffer-hook 'winner-mode)
(setq winner-boring-buffers '("*Completions*"
                              "*Compile-Log*"
                              "*inferior-lisp*"
                              "*Fuzzy Completions*"
                              "*Apropos*"
                              "*Help*"
                              "*cvs*"
                              "*Buffer List*"
                              "*Ibuffer*"
                              "*esh command on file*"))

(add-hook 'on-first-file-hook 'global-auto-revert-mode)

(use-package tab-bar
  :hook (on-first-file . tab-bar-mode)
  :config
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-tab-hints nil)
  (setq tab-bar-show nil))

(use-package tabspaces
  :after tab-bar
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

(add-to-list 'display-buffer-alist
             '((or (derived-mode . text-mode)
                   (derived-mode . ekg-notes-mode))
               (display-buffer-in-tab)
               (tab-name . "Edit") (tab-group . "Edit")
               (select . t)))

(add-to-list 'display-buffer-alist
             '((derived-mode . prog-mode)
               (display-buffer-in-tab)
               (tab-name . "Porg") (tab-group . "Prog")
               (select . t)))

(add-to-list 'display-buffer-alist
             '((or (derived-mode . dired-mode)
                   (derived-mode . dirvish-mode))
               (display-buffer-in-tab)
               (tab-name . "Dired")
               (tab-group . "Dired")))

(add-to-list 'display-buffer-alist
             `(,(rx (| "*dashboard*"
                       "*Messages*"))
               (display-buffer-in-tab)
               (tab-name . "Home")
               (tab-group . "Home")
               (window-parameters . ((mode-line-format . none)
                                     (no-other-window . t)))))

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

(add-hook 'text-mode-hook 'global-so-long-mode)
(setq-default large-file-warning-threshold nil)
(when (fboundp 'so-long-enable)
  (add-hook 'on-first-file-hook 'so-long-enable))

(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'org-mode-hook 'electric-pair-mode)

(setq prettify-symbols-alist '(("lambda" . ?λ)
                               ("function" . ?𝑓)))
(add-hook 'prog-mode-hook 'prettify-symbols-mode)
(add-hook 'LaTeX-mode-hook 'prettify-symbols-mode)

(setq hippie-expand-try-functions-list '(try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))
(global-set-key [remap dabbrev-expand] 'hippie-expand)

(add-hook 'prog-mode-hook 'outline-minor-mode)

(use-package loaddefs
  :hook (on-first-file . pixel-scroll-precision-mode))

(use-package recentf
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-save-file (expand-file-name "cache/recentf" user-emacs-directory))
  (setq recentf-auto-cleanup 300)
  (setq recentf-max-saved-items 1000))

(my/space-leader-def
  "fr" '(recentf-open-files :wk "Recentf"))

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
  (setq dired-auto-revert-buffer t))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (:map dired-mode-map
              ("s-." . dired-hide-dotfiles-mode)))

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         (:map minibuffer-mode-map
               ("C-x C-d" . consult-dir)
               ("C-x C-j" . consult-dir-jump-file))))

(use-package frame
  :config
  (face-spec-set 'window-divider
                 '((((background light))
                    :foreground "#000000")
                   (t
                    :foreground "#FFFFFF"))
                 'face-override-spec)
  (setq window-divider-default-bottom-width 1)
  (setq window-divider-default-places 'bottom-only)
  :hook (after-init . window-divider-mode))

(setq doc-view-mupdf-use-svg t)
(setq doc-view-imenu-flatten t)
(setq doc-view-continuous t)

(setq-default abbrev-mode t)

(use-package bookmark
  :config
  (setq bookmark-default-file (expand-file-name "cache/bookmarks" user-emacs-directory)))
(my/space-leader-def
  "ba" 'bookmark-set
  "br" 'bookmark-rename
  "bd" 'bookmark-delete
  "bj" 'bookmark-jump)

(setq select-enable-primary t)

(defun my/auto-create-missing-dirs ()
  (let ((target-dir (file-name-directory buffer-file-name)))
    (unless (file-exists-p target-dir)
      (make-directory target-dir t))))

(add-to-list 'find-file-not-found-functions #'my/auto-create-missing-dirs)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package url
  :config
  (setq url-configuration-directory (expand-file-name "cache/url" user-emacs-directory)))

(use-package multisession
  :config
  (setq multisession-directory (expand-file-name "cache/multisession" user-emacs-directory)))

(provide 'init-builtin)
;;; init-builtin.el ends here.
