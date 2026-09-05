;; -*- lexical-binding: t; -*-

;; Startup
(setq user-mail-address (getenv "MAIL_ACCOUNT"))

;; System Coding
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-language-environment "UTF-8")

(when (eq system-type 'windows-nt)
  ;; 文件名相关
  (setq file-name-coding-system 'gbk)
  (setq locale-coding-system 'gbk)
  (setq w32-unicode-filenames nil)
  ;; 识别优先级
  (prefer-coding-system 'gbk)
  (prefer-coding-system 'gb18030)
  (prefer-coding-system 'utf-8-unix)
  ;; 新建文件默认 UTF-8
  (setq-default buffer-file-coding-system 'utf-8-unix)
  (set-default-coding-systems 'utf-8-unix)
  ;; 子进程编码
  (setq default-process-coding-system '(gbk-dos . gbk-dos))
  ;; 更精确地针对 cmd / cmdproxy
  (modify-coding-system-alist 'process "[cC][mM][dD]"
			      '(gbk-dos . gbk-dos))
  (modify-coding-system-alist 'process "[cC][mM][dD][pP][rR][oO][xX][yY]"
			      '(gbk-dos . gbk-dos)))


;; Simple
(bind-key [remap downcase-word] #'downcase-dwim)
(bind-key [remap upcase-word] #'upcase-dwim)
(bind-key [remap capitalize-word] #'capitalize-dwim)

(setq use-short-answers t)
;;关闭 ring bell,用 mode-line 替代
(setq ring-bell-function (lambda ()
			   (invert-face 'mode-line)
			   (run-with-timer 0.05 nil 'invert-face 'mode-line)))
(setq create-lockfiles nil)		;不要创建 lockfiles
(setq history-delete-duplicates t)	;删除历史记录重复项
(setq delete-by-moving-to-trash t)	;删除文件至系统垃圾箱
(setq cursor-in-non-selected-windows nil) ;除当前窗口不显示光标
(setq highlight-nonselected-windows nil)
(setq read-buffer-completion-ignore-case t)
(setq inhibit-compacting-font-caches t)
(setq save-interprogram-paste-before-kill t)
(setq window-combination-resize t)

(setq ffap-machine-p-known 'reject)

(setq mark-ring-max 128
      kill-do-not-save-duplicates t
      kill-ring-max (* kill-ring-max 2)
      async-shell-command-display-buffer nil)

;; https://emacsredux.com/blog/2026/04/07/stealing-from-the-best-emacs-configs/
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

(setq redisplay-skip-fontification-on-input t)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(my/idle-loader-add '(which-key-mode)
		    '(global-goto-address-mode)
		    '(midnight-mode)
		    '(pixel-scroll-precision-mode)
		    '(delete-selection-mode)
		    '(global-word-wrap-whitespace-mode)
		    '(which-function-mode))

;; transient
(with-eval-after-load 'transient
  ;; (setq transient-show-popup 1)
  (setq transient-history-file (expand-file-name "transient/history.el" cache-directory)
	transient-levels-file (expand-file-name "transient/levels.el" cache-directory)
	transient-values-file (expand-file-name "transient/values.el" cache-directory)))

(with-eval-after-load 'url
  (setq url-configuration-directory (expand-file-name "url" cache-directory))
  (setq url-history-file (expand-file-name "history" url-configuration-directory))
  (setq url-cookie-file (expand-file-name "cookies" url-configuration-directory)))

(setq auto-save-default nil
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

(add-hook 'on-first-file-hook #'global-auto-revert-mode)

;; Scroll
(setq fast-but-imprecise-scrolling t
      scroll-step 6
      scroll-margin 6
      scroll-up-aggressively 0.25
      scroll-down-aggressively 0.25
      auto-window-vscroll t
      auto-hscroll-mode t
      hscroll-step 0.3
      hscroll-margin 6)

(with-eval-after-load 'pixel-scroll
  (setq pixel-scroll-precision-use-momentum t
	pixel-scroll-precision-large-scroll-height 40.0
	pixel-scroll-precision-interpolation-factor 2.0))

(add-hook 'prog-mode-hook #'subword-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'org-mode-hook #'display-line-numbers-mode)
(with-eval-after-load 'display-line-numbers
  (setq display-line-numbers-widen t
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
  (setq hl-line-sticky-flag nil))

(add-hook 'on-first-file-hook #'savehist-mode)
(with-eval-after-load 'savehist
  (setq savehist-file (expand-file-name "history" cache-directory)
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

(setq save-place-file (expand-file-name "places" cache-directory))
(setq save-place-autosave-interval (* 60 5))
(add-hook 'on-first-file-hook #'save-place-mode)
(advice-add 'save-place-find-file-hook :after
            (lambda (&rest _)
              (when buffer-file-name (ignore-errors (recenter)))))

(use-package recentf
  :idle t
  :bind ("C-x C-r" . recentf-open-files)
  :custom
  (recentf-max-saved-items 50)
  (recentf-keep nil)
  (recentf-autosave-interval 300)
  (recentf-show-messages nil)
  (recentf-save-file (expand-file-name "recentf" cache-directory))
  :config
  (add-to-list 'recentf-exclude #'recentf-exclude-file-by-extension-p)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name)
  (add-to-list 'recentf-filename-handlers #'substring-no-properties)
  (recentf-mode 1))

(add-hook 'prog-mode-hook #'electric-pair-mode)
(add-hook 'prog-mode-hook #'electric-indent-mode)
(add-hook 'text-mode-hook #'electric-quote-mode)
(add-hook 'prog-mode-hook #'electric-layout-mode)

(add-hook 'prog-mode-hook 'prettify-symbols-mode)
(add-hook 'prog-mode-hook (lambda ()
			    (setq-local prettify-symbols-alist '(("lambda" . ?λ)
								 ("function" . ?𝑓)))))

(add-hook 'prog-mode-hook 'show-paren-mode)
(with-eval-after-load 'paren
  (setq show-paren-style 'parenthesis
	show-paren-context-when-offscreen 'overlay
	show-paren-highlight-openparen t
	show-paren-when-point-inside-paren t
	show-paren-when-point-in-periphery t))

(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local whitespace-style
                        '(face trailing tabs tab-mark))
            (whitespace-mode 1)))

(defun my/delete-trailing-whitespace-except-current-line ()
  "Delete trailing whitespace, but keep the current line intact."
  (interactive)
  (let ((beg (point-min))
        (end (point-max))
        (bol (line-beginning-position))
        (eol (line-end-position)))
    (delete-trailing-whitespace beg bol)
    (delete-trailing-whitespace eol end)))

(defun auto-save-delete-trailing-whitespace-except-current-line ()
    (interactive)
    (let ((begin (line-beginning-position))
          (end (point))
          (buffername (buffer-name (buffer-base-buffer))))
      (when (not (or (string-prefix-p "inbox" buffername)
                     (string-match-p "^[0-9]" buffername)))
        (save-excursion
          (when (< (point-min) begin)
            (save-restriction
              (narrow-to-region (point-min) (1- begin))
              (delete-trailing-whitespace)))
          (when (> (point-max) end)
            (save-restriction
              (narrow-to-region end (point-max))
              (delete-trailing-whitespace)))))))

(add-hook 'before-save-hook #'auto-save-delete-trailing-whitespace-except-current-line)

(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

(add-hook 'on-first-input-hook #'repeat-mode)
(with-eval-after-load 'repeat
  (setq repeat-on-final-keystroke t
	repeat-exit-timeout 5
	repeat-exit-key "<escape>"
	repeat-keep-prefix nil
	repeat-check-key t
	set-mark-command-repeat-pop t))

(add-hook 'on-first-buffer-hook #'tab-bar-mode)
(with-eval-after-load 'tab-bar
  (setq tab-bar-auto-width nil
	tab-bar-new-tab-choice 'scratch-buffer
	tab-bar-close-button-show nil
	tab-bar-new-tab-to 'rightmost
	tab-bar-separator ""
	tab-bar-select-tab-modifiers '(super)
	tab-bar-tab-hints t
	tab-bar-truncate t
	tab-bar-show 1))


;; windmove
(add-hook 'on-first-input-hook #'windmove-mode)
(with-eval-after-load 'windmove
  (windmove-default-keybindings))

;; winner
(add-hook 'on-first-buffer-hook 'winner-mode)
;; (global-set-key (kbd "M-g u") #'winner-undo)
;; (global-set-key (kbd "M-g r") #'winner-redo)

;; (with-eval-after-load 'winner
;;   (defvar my-winner-repeat-map
;;     (let ((map (make-sparse-keymap)))
;;       (define-key map "u" #'winner-undo)
;;       (define-key map "r" #'winner-redo)
;;       map)
;;     "Winner-mode 连击按键映射.")

;;   ;; 2. 将命令关联到该映射
;;   (put 'winner-undo 'repeat-map 'my-winner-repeat-map)
;;   (put 'winner-redo 'repeat-map 'my-winner-repeat-map))

;; (with-eval-after-load 'winner
;;   (setopt winner-dont-bind-my-keys t
;; 	  winner-boring-buffers '("*Completions*"
;; 				  "*Compile-Log*"
;; 				  "*inferior-lisp*"
;; 				  "*Fuzzy Completions*"
;; 				  "*Apropos*"
;; 				  "*Help*"
;; 				  "*cvs*"
;; 				  "*Buffer List*"
;; 				  "*Ibuffer*"
;; 				  "*esh command on file*")))

(defun toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key (kbd "C-x 1") #'toggle-delete-other-windows)

(with-eval-after-load 'which-key
  (setq which-key-idle-delay 0.1
	which-key-show-remaining-keys t))


(provide 'init-builtin)
