;; -*- lexical-binding: t; -*-

;; C Source code
(setq use-short-answers t)
(setq ring-bell-function (lambda ()
			   (invert-face 'mode-line)
			   (run-with-timer 0.05 nil 'invert-face 'mode-line)))
(setq create-lockfiles nil)
(setq history-delete-duplicates t)
(setq delete-by-moving-to-trash t)
(setq cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq read-buffer-completion-ignore-case t)
(setq inhibit-compacting-font-caches t)
(setq save-interprogram-paste-before-kill t)
(setq window-combination-resize t)
(setq ffap-machine-p-known 'reject)
(setq multisession-directory (expand-file-name "multisession" cache-directory))

;; https://emacsredux.com/blog/2026/04/07/stealing-from-the-best-emacs-configs/
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

(setq redisplay-skip-fontification-on-input t)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t
      scroll-step 6
      scroll-margin 6
      scroll-up-aggressively 0.25
      scroll-down-aggressively 0.25
      auto-window-vscroll t
      auto-hscroll-mode t
      hscroll-step 0.3
      hscroll-margin 6)

;; minibuffer
(setq enable-recursive-minibuffers t)
(setq read-minibuffer-restore-windows nil)
(setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
(setq minibuffer-follows-selected-frame nil)
(setq minibuffer-completion-confirm 'confirm)
(setq completion-ignore-case t)

;; Startup
(setq user-mail-address (getenv "MAIL_ACCOUNT"))
(with-eval-after-load 'startup
  (setq auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" cache-directory)))

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


;; jit-lock
(with-eval-after-load 'jit-lock
  (setq jit-lock-defer-time 0)
  (setq jit-lock-stealth-time 0.5)
  (setq jit-lock-stealth-nice 0.5)
  (setq jit-lock-stealth-load 100)
  (setq jit-lock-chunk-size 1024))

;; eldoc
(with-eval-after-load 'eldoc
  (setq eldoc-help-at-pt t)
  (setq eldoc-idle-delay 0.5)
  (setq eldoc-idle-delay-visible-only t)
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eldoc-documentation-strategy 'eldoc-documentation-enthusiast))

;; Simple
(bind-key [remap downcase-word] #'downcase-dwim)
(bind-key [remap upcase-word] #'upcase-dwim)
(bind-key [remap capitalize-word] #'capitalize-dwim)
(add-hook 'completion-list-mode-hook (lambda () (setq-local truncate-lines t)))
(with-eval-after-load 'simple
  (setq completion-auto-select t)
  (setq completion-show-help nil)
  (setq completion-auto-wrap nil)
  (setq kill-whole-line t)
  (setq kill-region-dwim t)
  (setq track-eol t)
  (setq kill-read-only-ok t)
  (setq mark-ring-max 128)
  (setq kill-do-not-save-duplicates t)
  (setq kill-ring-max (* kill-ring-max 2))
  (setq async-shell-command-display-buffer nil))

;; Server
(add-hook 'on-first-file-hook #'server-start)
(with-eval-after-load 'server
  (setq server-client-instructions nil))

;; files
(with-eval-after-load 'files
  (setq auto-save-default nil
	auto-save-visited-interval 1
	save-silently t
	large-file-warning-threshold nil
	confirm-kill-processes nil
	confirm-kill-emacs nil
	make-backup-files nil
	view-read-only t))

(add-hook 'on-first-file-hook #'auto-save-visited-mode)
(add-hook 'before-save-hook #'auto-save-delete-trailing-whitespace-except-current-line)
(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

;; indent
(with-eval-after-load 'indent
  (setq tab-always-indent 'complete)
  (setq tab-first-completion 'word-or-paren-or-punct))

;; isearch
(with-eval-after-load 'isearch
  (setq isearch-lazy-count t))

;; help
(with-eval-after-load 'help
  (setq help-window-select 'other)
  (setq help-window-keep-selected t))

;; loaddefs
(with-eval-after-load 'loaddefs
  (setq ad-redefinition-action 'accept))

;; mule-util
(with-eval-after-load 'mule-util
  (setq truncate-string-ellipsis "…"))

;; Auto revert
(add-hook 'on-first-file-hook #'global-auto-revert-mode)
(with-eval-after-load 'autorevert
  (setq auto-revert-avoid-polling t)
  (setq auto-revert-stop-on-user-input nil))

;; Scroll
(with-eval-after-load 'pixel-scroll
  (setq pixel-scroll-precision-use-momentum t
	pixel-scroll-precision-large-scroll-height 40.0
	pixel-scroll-precision-interpolation-factor 2.0))

;; subword
(add-hook 'prog-mode-hook #'subword-mode)

;; display-line-numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'org-mode-hook #'display-line-numbers-mode)
(with-eval-after-load 'display-line-numbers
  (setq display-line-numbers-widen t
	display-line-numbers-type 'relative))

;; display-fill-column-indicator
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(with-eval-after-load 'display-fill-column-indicator
  (face-spec-set 'fill-column-indicator
		 '((default :height 0.1))
		 'face-override-spec)
  (setq-default fill-column 90))

;; visual-line-mode
(add-hook 'org-mode-hook #'visual-line-mode)
(add-hook 'eww-mode #'visual-line-mode)

;; hl-line
(when (display-graphic-p)
  (add-hook 'prog-mode-hook #'hl-line-mode)
  (add-hook 'package-menu-mode #'hl-line-mode))
(with-eval-after-load 'hl-line
  (setq hl-line-sticky-flag nil))

;; savehist
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

;; saveplace
(setq save-place-file (expand-file-name "places" cache-directory))
(setq save-place-autosave-interval (* 60 5))
(add-hook 'on-first-file-hook #'save-place-mode)
(advice-add 'save-place-find-file-hook :after
            (lambda (&rest _)
              (when buffer-file-name (ignore-errors (recenter)))))

;; recentf
(add-hook 'on-first-buffer-hook #'recentf-mode)
(keymap-global-set "C-x C-r" #'recentf-open-files)
(with-eval-after-load 'recentf
  (setq recentf-max-saved-items 50
	recentf-keep nil
	recentf-autosave-interval 300
	recentf-show-messages nil
	recentf-save-file (expand-file-name "recentf" cache-directory))
  (add-to-list 'recentf-exclude #'recentf-exclude-file-by-extension-p)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name)
  (add-to-list 'recentf-filename-handlers #'substring-no-properties))

;; electric
(add-hook 'prog-mode-hook #'electric-pair-mode)
(add-hook 'prog-mode-hook #'electric-indent-mode)
(add-hook 'text-mode-hook #'electric-quote-mode)
(add-hook 'prog-mode-hook #'electric-layout-mode)

;; prettify-symbols-mode
(add-hook 'prog-mode-hook 'prettify-symbols-mode)
(add-hook 'prog-mode-hook (lambda ()
			    (setq-local prettify-symbols-alist '(("lambda" . ?λ)
								 ("function" . ?𝑓)))))

;; paren
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

;; repeat
(add-hook 'on-first-input-hook #'repeat-mode)
(with-eval-after-load 'repeat
  (setq repeat-on-final-keystroke t
	repeat-exit-timeout 5
	repeat-exit-key "<escape>"
	repeat-keep-prefix nil
	repeat-check-key t
	set-mark-command-repeat-pop t))

;; tab-bar
(add-hook 'on-first-input-hook #'tab-bar-mode)
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
(with-eval-after-load 'winner
  (defvar my-winner-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map "u" #'winner-undo)
      (define-key map "r" #'winner-redo)
      map)
    "Winner-mode 连击按键映射.")

  ;; 2. 将命令关联到该映射
  (put 'winner-undo 'repeat-map 'my-winner-repeat-map)
  (put 'winner-redo 'repeat-map 'my-winner-repeat-map))

(with-eval-after-load 'winner
  (setq winner-dont-bind-my-keys t
	winner-boring-buffers '("*Completions*"
				"*Compile-Log*"
				"*inferior-lisp*"
				"*Fuzzy Completions*"
				"*Apropos*"
				"*Help*"
				"*cvs*"
				"*Buffer List*"
				"*Ibuffer*"
				"*esh command on file*")))


;; which-key-mode
(add-hook 'on-first-input-hook #'which-key-mode)
(with-eval-after-load 'which-key
  (setq which-key-idle-delay 0.1
	which-key-show-remaining-keys t))

;; transient
(with-eval-after-load 'transient
  (setq transient-show-popup 1)
  (setq transient-history-file (expand-file-name "transient/history.el" cache-directory)
	transient-levels-file (expand-file-name "transient/levels.el" cache-directory)
	transient-values-file (expand-file-name "transient/values.el" cache-directory)))

;; url
(with-eval-after-load 'url
  (setq url-configuration-directory (expand-file-name "url" cache-directory))
  (setq url-history-file (expand-file-name "history" url-configuration-directory))
  (setq url-cookie-file (expand-file-name "cookies" url-configuration-directory)))

(my/idle-loader-add '(global-goto-address-mode)
		    '(midnight-mode)
		    '(pixel-scroll-precision-mode)
		    '(delete-selection-mode)
		    '(global-word-wrap-whitespace-mode)
		    '(which-function-mode))


(provide 'init-builtin)
