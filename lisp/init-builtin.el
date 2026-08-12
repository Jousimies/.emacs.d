;; -*- lexical-binding: t; -*-

(setopt use-short-answers t)
;关闭 ring bell,用 mode-line 替代
;; (setopt ring-bell-function (lambda ()
;; 			     (invert-face 'mode-line)
;; 			     (run-with-timer 0.05 nil 'invert-face 'mode-line)))
(setopt create-lockfiles nil)		;不要创建 lockfiles
(setopt history-delete-duplicates t)	;删除历史记录重复项
(setopt delete-by-moving-to-trash t)	;删除文件至系统垃圾箱
(setopt cursor-in-non-selected-windows nil) ;除当前窗口不显示光标
(setopt highlight-nonselected-windows nil)
(setopt read-buffer-completion-ignore-case t)
(setopt read-process-output-max (* 4 1024 1024))
(setopt inhibit-compacting-font-caches t)
(setopt save-interprogram-paste-before-kill t)
(setopt lexical-binding t)
(setopt window-combination-resize t)

(setopt mark-ring-max 128
	kill-do-not-save-duplicates t
	kill-ring-max (* kill-ring-max 2)
	async-shell-command-display-buffer nil)

(add-hook 'on-first-buffer-hook #'global-so-long-mode)

;; transient
(with-eval-after-load 'transient
  ;; (setopt transient-show-popup 1)
  (setq transient-history-file (expand-file-name "transient/history.el" cache-directory)
	transient-levels-file (expand-file-name "transient/levels.el" cache-directory)
	transient-values-file (expand-file-name "transient/values.el" cache-directory)))

(setopt url-configuration-directory (expand-file-name "url" cache-directory))
(setopt url-history-file (expand-file-name "history" url-configuration-directory))
(setopt url-cookie-file (expand-file-name "cookies" url-configuration-directory))

(setopt auto-save-default nil
        auto-save-visited-interval 1
        save-silently t
        large-file-warning-threshold nil
        confirm-kill-processes nil
        confirm-kill-emacs nil
        make-backup-files nil
        view-read-only t
        kill-read-only-ok t
	isearch-lazy-count t
	help-window-select 'other
	help-window-keep-selected t
	ad-redefinition-action 'accept
	truncate-string-ellipsis "…"
	multisession-directory (expand-file-name "multisession" cache-directory)
	auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" cache-directory))

(add-hook 'on-first-file-hook #'auto-save-visited-mode)

;; https://emacs-china.org/t/macos-save-silently-t/24086
(setq inhibit-message-regexps '("^Saving" "^Wrote"))
(setq set-message-functions '(inhibit-message))

(add-hook 'find-file-hook #'global-auto-revert-mode)

(add-hook 'on-first-file-hook #'midnight-mode)

(add-hook 'on-first-buffer-hook 'pixel-scroll-precision-mode)

(with-eval-after-load 'pixel-scroll
  (setopt pixel-scroll-precision-use-momentum t
	  pixel-scroll-precision-large-scroll-height 40.0
	  pixel-scroll-precision-interpolation-factor 2.0))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'org-mode-hook #'display-line-numbers-mode)
(with-eval-after-load 'display-line-numbers
  (setopt display-line-numbers-widen t
	  display-line-numbers-type 'relative))

(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(with-eval-after-load 'display-fill-column-indicator
  (face-spec-set 'fill-column-indicator
		 '((default :height 0.1))
		 'face-override-spec)
  (setq-default fill-column 90))

(add-hook 'org-mode-hook #'visual-line-mode)
(add-hook 'eww-mode #'visual-line-mode)

(when (display-graphic-p)
  (add-hook 'prog-mode-hook #'hl-line-mode)
  (add-hook 'package-menu-mode #'hl-line-mode))
(with-eval-after-load 'hl-line
  (setopt hl-line-sticky-flag nil))

(add-hook 'window-setup-hook #'savehist-mode)
(with-eval-after-load 'savehist
  (setopt savehist-file (expand-file-name "history" cache-directory)
	  history-length 1000
          savehist-additional-variables '(kill-ring
                                          search-ring
                                          regexp-search-ring)
          history-delete-duplicates t)
  (add-hook 'savehist-save-hook
            (lambda ()
              (setq kill-ring
                    (mapcar #'substring-no-properties
                            (cl-remove-if-not #'stringp kill-ring))))))

(setopt save-place-file (expand-file-name "places" cache-directory))
(setopt save-place-autosave-interval (* 60 5))
(add-hook 'after-init-hook #'save-place-mode)
;; https://emacsredux.com/blog/2026/04/07/stealing-from-the-best-emacs-configs/
(advice-add 'save-place-find-file-hook :after
            (lambda (&rest _)
              (when buffer-file-name (ignore-errors (recenter)))))

(setup recentf
  (:hook-into on-first-buffer-hook)
  (:when-loaded
    (setopt recentf-max-saved-items 50
            recentf-keep nil
            recentf-autosave-interval 300
            recentf-show-messages nil)
    (add-to-list 'recentf-exclude #'recentf-exclude-file-by-extension-p)
    ;; Add dired directories to recentf file list.
    (:with-mode dired-mode
      (:hook (lambda () (recentf-add-file default-directory))))
    (add-to-list 'recentf-filename-handlers #'abbreviate-file-name)
    ;; HACK: Text properties inflate the size of recentf's files, and there is
    ;; no purpose in persisting them (Must be first in the list!)
    (add-to-list 'recentf-filename-handlers #'substring-no-properties)))

(add-hook 'prog-mode-hook #'electric-pair-mode)
(add-hook 'prog-mode-hook #'electric-indent-mode)

(provide 'init-builtin)
