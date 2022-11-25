;;; init-builtin.el --- Builtin Emacs.     -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; Coding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

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
;; profiler
(add-hook 'profiler-report-mode-hook #'hl-line-mode)

;; show column number in modeline.
(add-hook 'prog-mode-hook 'column-number-mode)

;; Delete selection
(add-hook 'on-first-input-hook 'delete-selection-mode)

;; Winner.
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

;; Auto revert file.
(add-hook 'on-first-file-hook 'global-auto-revert-mode)

;; Save history.
(setq-default history-length 1000
              savehist-save-minibuffer-history 1
              savehist-additional-variables '(kill-ring
                                              search-ring
                                              regexp-search-ring)
              history-delete-duplicates t)
;; (run-with-idle-timer 1 nil (lambda ()
;;                              (savehist-mode)))
(add-hook 'on-first-file-hook 'savehist-mode)

;; Save place.
(add-hook 'after-init-hook 'save-place-mode)

;;
(add-hook 'after-init-hook 'midnight-mode)

;;
(add-hook 'text-mode-hook 'global-so-long-mode)

;; Large file.
(setq-default large-file-warning-threshold nil)
(when (fboundp 'so-long-enable)
  (add-hook 'after-init-hook 'so-long-enable))

;; Display time in modeline.
;; (with-eval-after-load 'time
;;   (setq display-time-24hr-format t)
;;   (setq display-time-format "%m/%d %H:%M %a")
;;   (setq display-time-load-average-threshold nil))

;; (add-hook 'after-init-hook 'display-time-mode 20)

;; Display battery in modeline.
;; (setq battery-load-critical 15)
;; (setq battery-mode-line-format " %b%p% ")
;; (add-hook 'after-init-hook 'display-battery-mode 10)

;; Auto insert pair.
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'org-mode-hook 'electric-pair-mode)

;; Pretty symbols.
(setq prettify-symbols-alist '(("lambda" . ?λ)
                               ("function" . ?𝑓)))
(add-hook 'prog-mode-hook 'prettify-symbols-mode)

;; Hippie expand.
(setq hippie-expand-try-functions-list '(try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; outline-mode
(add-hook 'prog-mode-hook 'outline-minor-mode)

;; pixel-scroll-precision-mode
(if (version< "29" emacs-version)
    (pixel-scroll-precision-mode))


(provide 'init-builtin)
;;; init-builtin.el ends here
