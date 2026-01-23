;;; init.el --- Emacs Initial Configuration          -*- lexical-binding: t; -*-

(add-to-list 'load-path "~/.emacs.d/packages/dash.el/")
(add-to-list 'load-path "~/.emacs.d/packages/f.el/")
(add-to-list 'load-path "~/.emacs.d/packages/s.el/")

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024)
                  gc-cons-percentage 0.1)))

(use-package gcmh
  :load-path "~/.emacs.d/packages/gcmh/"
  :hook (after-init . gcmh-mode)
  :custom
  (gc-cons-percentage 0.1)
  (gcmh-verbose nil)
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold #x1000000))

(advice-add 'after-focus-change-function :after 'garbage-collect)

(set-language-environment "UTF-8")	;中文
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setopt ring-bell-function (lambda ()
			     (invert-face 'mode-line)
			     (run-with-timer 0.05 nil 'invert-face 'mode-line))) ;关闭 ring bell,用 mode-line 替代
(setopt use-short-answers t)		;用 y/n 替代 yes/no
(setopt create-lockfiles nil)		;不要创建 lockfiles
(setopt history-delete-duplicates t)	;删除历史记录重复项
(setopt delete-by-moving-to-trash t)	;删除文件至系统垃圾箱
(setopt cursor-in-non-selected-windows nil) ;除当前窗口不显示光标
(setopt read-buffer-completion-ignore-case t)
(setopt ns-use-proxy-icon nil)
(setopt ns-use-srgb-colorspace nil)

(setopt enable-recursive-minibuffers t)
(setopt read-minibuffer-restore-windows nil
  	minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(defvar nextcloud "~/Nextcloud"
  "This folder is My cloud.")

(defvar icloud "~/Library/Mobile Documents/"
  "This folder contains documents in icloud.")

(defvar my-galaxy (expand-file-name "L.Personal.Galaxy" nextcloud)
  "This folder stores all the plain text files of my life.")

(defvar website-directory (expand-file-name "blogs_source/" my-galaxy)
  "The source folder of my blog.")

(defvar my-pictures (expand-file-name "pictures/" my-galaxy)
    "The folder save pictures.")

(defvar my-web_archive (expand-file-name "web_archive/" my-galaxy)
    "The folder save web pages.")

(defvar cache-directory (expand-file-name ".cache" user-emacs-directory))

(use-package dashboard
  :load-path "~/.emacs.d/packages/emacs-dashboard/"
  :demand t
  :hook (dashboard-mode . (lambda ()
			    (setq-local display-line-numbers nil)))
  :bind (:map dashboard-mode-map
			  ("n" . dashboard-next-line)
			  ("p" . dashboard-previous-line))
  :custom
  (dashboard-startup-banner (expand-file-name "src/bitmap.png" user-emacs-directory))
  (dashboard-image-banner-max-width 500)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-file-icons t)
  (dashboard-center-content t)
  (dashboard-set-init-info t)
  (dashboard-week-agenda nil)
  (dashboard-set-footer nil)
  (dashboard-items '((recents  . 5)
                     (bookmarks . 5)
                     (registers . 5)))
  (dashboard-banner-logo-title "EXPLORE THE WORLD, FULFILL YOUR BEING.")
  :config
  (dashboard-setup-startup-hook))

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

(when (featurep 'ns)
  (defun my/apply-theme (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (load-theme 'modus-operandi-deuteranopia t))
      ('dark (load-theme 'modus-vivendi-deuteranopia t))))
  (add-hook 'ns-system-appearance-change-functions #'my/apply-theme))

;; 设置中文字集
;; `han': 汉字字符集，主要用于简体中文和繁体中文字符
;; `cjk-misc': CJK（中日韩）字符集中的其他字符，包含了少量的中文、日文、韩文字符
;; `kana': 日文假名字符集，但在处理与中文相关的文档时可能偶尔用到
;; `bopomofo': 注音符号字符集，用于台湾地区的汉字注音

(setopt use-default-font-for-symbols nil)

(when IS-MAC
  (set-face-attribute 'default nil :family "Cascadia Next SC" :height 140))

(when (and IS-MAC (display-graphic-p))
  (dolist (charset '(kana han cjk-misc bopomofo symbol))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family "SimSun"))))

(when IS-WINDOWS
  (dolist (charset '(kana han cjk-misc bopomofo symbol))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family "SimHei"))))

(when (display-graphic-p)
  (set-fontset-font t 'unicode (font-spec :family "Symbols Nerd Font Mono" :size 14) nil 'prepend))

(when (display-graphic-p)
  (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji" :size 14) nil 'prepend)
  (set-fontset-font t 'symbol (font-spec :family "Symbols" :size 14) nil 'prepend))

(use-package nerd-icons
  :load-path "~/.emacs.d/packages/nerd-icons.el/"
  :commands nerd-icons-codicon nerd-icons-faicon nerd-icons-icon-for-file
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-ibuffer
  :load-path "~/.emacs.d/packages/nerd-icons-ibuffer/"
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-dired
  :load-path "~/.emacs.d/packages/nerd-icons-dired/"
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :load-path "~/.emacs.d/packages/nerd-icons-completion/"
  :hook (minibuffer-setup . nerd-icons-completion-mode))

(add-hook 'input-method-activate-hook (lambda ()
                                        (setq cursor-type 'bar)))
(add-hook 'input-method-deactivate-hook (lambda ()
                                          (setq cursor-type 'box)))

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

(add-hook 'after-init-hook #'auto-save-visited-mode)

;; https://emacs-china.org/t/macos-save-silently-t/24086
(setq inhibit-message-regexps '("^Saving" "^Wrote"))
(setq set-message-functions '(inhibit-message))

(add-hook 'after-init-hook #'global-auto-revert-mode)

(setq bookmark-default-file (expand-file-name "bookmarks" cache-directory))

(setopt trash-directory "~/.Trash")

(add-hook 'after-init-hook #'midnight-mode)

(add-hook 'after-init-hook 'pixel-scroll-precision-mode)

(with-eval-after-load 'pixel-scroll
 (setopt pixel-scroll-precision-use-momentum t
	pixel-scroll-precision-large-scroll-height 40.0
	pixel-scroll-precision-interpolation-factor 2.0))

(add-hook 'after-init-hook #'recentf-mode)
(with-eval-after-load 'recentf
  (setopt recentf-save-file (expand-file-name "recentf" cache-directory)
	  recentf-auto-cleanup 300
	  recentf-max-saved-items 1000
	  recentf-exclude '("~/.telega")))

(add-hook 'after-init-hook #'savehist-mode)
(with-eval-after-load 'savehist
  (setopt savehist-file (expand-file-name "history" cache-directory)
	  history-length 1000
          savehist-additional-variables '(kill-ring
                                          search-ring
                                          regexp-search-ring)
          history-delete-duplicates t))

(setopt save-place-file (expand-file-name "places" cache-directory))
(setopt save-place-autosave-interval (* 60 5))
(add-hook 'after-init-hook #'save-place-mode)

(setopt mark-ring-max 128
  	kill-do-not-save-duplicates t
  	kill-ring-max (* kill-ring-max 2)
  	async-shell-command-display-buffer nil)

(add-hook 'after-init-hook #'global-so-long-mode)

(setopt inhibit-startup-screen t)	;关闭启动页

(with-eval-after-load 'transient
  (setopt transient-show-popup 1)
  (setq transient-history-file (expand-file-name "transient/history.el" cache-directory)
	transient-levels-file (expand-file-name "transient/levels.el" cache-directory)
	transient-values-file (expand-file-name "transient/values.el" cache-directory)))

(setopt url-configuration-directory (expand-file-name "url" cache-directory))
(setopt url-history-file (expand-file-name "history" url-configuration-directory))
(setopt url-cookie-file (expand-file-name "cookies" url-configuration-directory))

(setopt minibuffer-completion-auto-choose t
	minibuffer-completion-confirm 'confirm)
(setopt tab-always-indent 'complete
	tab-first-completion 'word-or-paren-or-punct)

(keymap-set minibuffer-mode-map "C-r" #'minibuffer-complete-history)
(add-hook 'minibuffer-mode-hook #'minibuffer-electric-default-mode)
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(add-hook 'minibuffer-setup-hook (lambda ()
  				   (setq-local truncate-lines t)))
(define-key minibuffer-local-completion-map (kbd "C-n") #'icomplete-forward-completions)
(define-key minibuffer-local-completion-map (kbd "C-p") #'icomplete-backward-completions)

(setopt	completion-cycle-threshold 2
	completions-detailed t
        completions-format 'one-column
        completion-auto-select t
        completion-ignore-case t
        completion-show-inline-help nil
        completions-max-height 50
        completion-show-help nil
        completion-auto-wrap nil
        completions-header-format (propertize "%s candidates:\n" 'face 'font-lock-comment-face)
        completions-highlight-face 'completions-highlight)

(defun crm-indicator (args)
  (cons (format "[`completing-read-multiple': %s]  %s"
                (propertize
                 (replace-regexp-in-string
                  "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                  crm-separator)
                 'face 'error)
                (car args))
        (cdr args)))

(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

(add-hook 'minibuffer-mode-hook #'minibuffer-electric-default-mode)

(add-hook 'after-init-hook #'icomplete-mode)
(with-eval-after-load 'icomplete
  (setopt icomplete-delay-completions-threshold 0
	  icomplete-show-matches-on-no-input t
	  icomplete-hide-common-prefix nil
	  icomplete-separator "  |  "
	  icomplete-max-delay-chars 0
	  icomplete-compute-delay 0
	  )

  ;; (define-key icomplete-minibuffer-map (kbd "C-n") #'minibuffer-next-completion)
  ;; (define-key icomplete-minibuffer-map (kbd "C-p") #'minibuffer-previous-completion)
  ;; (define-key icomplete-minibuffer-map (kbd "C-j") #'icomplete-force-complete-and-exit)
  )

(add-hook 'prog-mode-hook #'completion-preview-mode)
(add-hook 'org-mode-hook #'completion-preview-mode)
(add-hook 'comint-mode-hook #'completion-preview-mode)

(with-eval-after-load 'completion-preview
  (define-key completion-preview-active-mode-map (kbd "TAB") #'completion-preview-insert)
  (define-key completion-preview-active-mode-map (kbd "C-n") #'completion-preview-next-candidate)
  (define-key completion-preview-active-mode-map (kbd "C-p") #'completion-preview-prev-candidate))

(use-package orderless
  :load-path "~/.emacs.d/packages/orderless/"
  :custom
  (orderless-matching-styles '(orderless-prefixes orderless-regexp))
  (completion-styles '(basic substring initials orderless))
  (completion-pcm-leading-wildcard t)
  (completions-format 'vertical)
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles . (basic partial-completion orderless)))
     (bookmark (styles . (basic substring)))
     (library (styles . (basic substring)))
     (embark-keybinding (styles . (basic substring)))
     (imenu (styles . (basic substring orderless)))
     (consult-location (styles . (basic substring orderless)))
     (kill-ring (styles . (emacs22 orderless)))
     (eglot (styles . (emacs22 substring orderless))))))

(use-package consult
  :load-path "~/.emacs.d/packages/consult/"
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (([remap apropos] . consult-apropos)
         ([remap bookmark-jump] . consult-bookmark)
         ([remap goto-line] . consult-line)
         ([remap locate] . consult-locate)
         ([remap load-theme] . consult-theme)
         ([remap man] . consult-man)
         ([remap recentf-open-files] . consult-recent-file)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap yank-pop] . consult-yank-pop)
	 ([remap imenu] . consult-imenu)
	 ("M-g l" . consult-goto-line)
         :map minibuffer-mode-map
         ("C-r" . consult-history))
  :custom
  (consult-narrow-key "<")
  (consult-preview-key "M-."))

;; Bind org-mode keys safely
(with-eval-after-load 'org
  (require 'consult-org)
  (define-key org-mode-map (kbd "M-g h") #'consult-org-heading))

(use-package marginalia
  :load-path "~/.emacs.d/packages/marginalia/"
  :hook (after-init . marginalia-mode))

(use-package consult-dir
  :load-path "~/.emacs.d/packages/consult-dir"
  :bind (([remap list-directory] . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package embark
  :load-path "~/.emacs.d/packages/embark/"
  :bind (([remap describe-bindings] . embark-bindings)
         ("C-;" . embark-act)
         ("M-." . embark-dwim)
         (:map minibuffer-local-map
               ("C-;" . embark-act)
               ("C-c C-e" . embark-export)
               ("C-c C-l" . embark-collect))))

(use-package embark-consult
  :after consult)

(use-package cape
  :load-path "~/.emacs.d/packages/cape"
  :bind ("C-c p" . cape-prefix-map)
  :commands cape-elisp-block
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(add-hook 'after-init-hook #'electric-indent-mode)

(add-hook 'after-init-hook #'electric-pair-mode)

(define-key global-map [remap dabbrev-expand] #'hippie-expand)
(with-eval-after-load 'hippie-exp
  (setopt hippie-expand-try-functions-list '(try-complete-file-name-partially
					     try-complete-file-name
					     try-expand-all-abbrevs
					     try-expand-dabbrev
					     try-expand-dabbrev-all-buffers
					     try-expand-dabbrev-from-kill
					     try-complete-lisp-symbol-partially
					     try-complete-lisp-symbol)))

(add-hook 'after-init-hook #'delete-selection-mode)

(add-hook 'find-file-hook 'show-paren-mode)
(with-eval-after-load 'paren
  (setopt show-paren-style 'parenthesis
	  show-paren-context-when-offscreen 'overlay
	  show-paren-highlight-openparen t
	  show-paren-when-point-inside-paren t
	  show-paren-when-point-in-periphery t))

(add-hook 'after-init-hook #'global-word-wrap-whitespace-mode)

(add-hook 'after-init-hook #'whitespace-mode)
(add-hook 'after-init-hook #'delete-trailing-whitespace-mode)

(with-eval-after-load 'register
  (setopt register-preview-delay 0)
  (set-register ?a (cons 'file (concat icloud "iCloud~com~appsonthemove~beorg/Documents/org/gtd_archive_" (format-time-string "%Y"))))
  (set-register ?f (cons 'file (concat icloud "iCloud~com~appsonthemove~beorg/Documents/org/flash_thoughts_" (format-time-string "%Y"))))
  (set-register ?t (cons 'file (expand-file-name "iCloud~com~appsonthemove~beorg/Documents/org/org-gtd-tasks.org" icloud)))
  (set-register ?r (cons 'file (expand-file-name (format-time-string "logs/weekly_review_%Y.org") my-galaxy)))
  (set-register ?l (cons 'file (expand-file-name (format-time-string "logs/work_log_%Y.org") my-galaxy))))

(use-package expand-region
  :load-path "~/.emacs.d/packages/expand-region.el/"
  :bind ("C-=" . er/expand-region)
  :config
  (add-to-list 'expand-region-exclude-text-mode-expansions 'org-mode)
  (add-to-list 'expand-region-exclude-text-mode-expansions 'LaTeX-mode))

(use-package surround
  :load-path "~/.emacs.d/packages/surround/"
  :commands surround-delete surround-change surround-insert)

(use-package selected
  :load-path "~/.emacs.d/packages/selected.el/"
  :hook (post-select-region . selected-minor-mode)
  :bind (:map selected-keymap
              ("q" . selected-off)
              ("x" . kill-region)
              ("w" . count-words-region)
              ("i" . surround-insert)
              ("c" . surrond-change)
	      ("d" . surround-delete)
              ("s" . my/search-menu)
              ("m" . apply-macro-to-region-lines)
              ("\\" . indent-region)
              (";" . comment-dwim)))

(use-package symbol-overlay
  :load-path "~/.emacs.d/packages/symbol-overlay/"
  :hook ((prog-mode . symbol-overlay-mode)
         (html-mode . symbol-overlay-mode))
  :bind (:map symbol-overlay-mode-map
              ("M-i" . symbol-overlay-put)
              ("M-I" . symbol-overlay-remove-all)))

(use-package rainbow-mode
  :load-path "~/.emacs.d/packages/rainbow-mode/"
  :hook (prog-mode . rainbow-mode))

(use-package goggles
  :load-path "~/.emacs.d/packages/goggles/"
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t))

(use-package undo-fu-session
  :load-path "~/.emacs.d/packages/undo-fu-session/"
  :hook (after-init . undo-fu-session-global-mode)
  :custom
  (undo-fu-session-directory (expand-file-name "undo-fu-session/" cache-directory)))

(with-eval-after-load 'undo-fu-session
  (defun my/undo-fu-session--make-file-name (filename)
    "Take the path FILENAME and return a name base on this."
    (concat
     (file-name-concat undo-fu-session-directory
                       (md5 (convert-standard-filename (expand-file-name filename))))
     (undo-fu-session--file-name-ext)))
  (advice-add 'undo-fu-session--make-file-name :override #'my/undo-fu-session--make-file-name))

(use-package vundo
  :load-path "~/.emacs.d/packages/vundo/"
  :bind ("s-z" . vundo)
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))

;; Make region read-only or writable
(defun make-region-read-only (beg end)
  (interactive "r")
  (let ((inhibit-read-only t))
    (with-silent-modifications
      (add-text-properties beg end '(read-only t)))))

(defun make-region-writable (beg end)
  (interactive "r")
  (let ((inhibit-read-only t))
    (with-silent-modifications
      (remove-text-properties beg end '(read-only t)))))

(use-package emacs-everywhere
  :load-path "~/.emacs.d/packages/emacs-everywhere/"
  :commands emacs-everywhere)

(use-package rime
  :load-path "~/.emacs.d/packages/emacs-rime/"
  :if IS-MAC
  :config
  (setq default-input-method "rime")
  (setq rime-librime-root "/opt/homebrew")
  ;; (setq rime-librime-root (expand-file-name "librime/dist" user-emacs-directory))
  (setq rime-emacs-module-header-root "/Applications/Emacs.app/Contents/Resources/include/")
  (setq rime-user-data-dir "~/Library/Rime/"))

(use-package rime-regexp
  :load-path "~/.emacs.d/packages/rime-regexp.el/"
  :hook (minibuffer-mode . (lambda ()
			     (progn
			       (rime-regexp-load-rime)
			       (advice-add 'orderless-regexp :filter-args #'rime-regexp-filter-args)))))

(use-package macim
  :load-path "~/.emacs.d/packages/macim.el/"
  :hook ((after-init . macim-mode)
	 (minibuffer-setup-mode . macim-select-ascii)
	 (isearch-mode . macim-select-ascii))
  :custom
  (macim-other "im.rime.inputmethod.Squirrel.Hans")
  :config
  (advice-add 'select-window :after #'(lambda (&rest _) (macim-context-switch)))
  (defvar my/macim-context-ignore-modes '("telega-root-mode"
					  "telega-image-mode"
					  "mu4e-headers-mode"
					  "mu4e-view-mode"
					  "elfeed-show-mode"
					  "elfeed-search-mode"))
  (defun +macim-context-ignore-modes ()
    (let ((mode (symbol-name major-mode)))
      (when (member mode my/macim-context-ignore-modes))
      'ascii))

  (add-to-list 'macim-context-early-predicates #'+macim-context-ignore-modes)

  ;; Trim excess spaces on both sides on deactivation
  (setq +macim-chinese-punc-chars (mapcar #'string-to-char macim--chinese-punc-list))

  (defun +macim-remove-head-space-after-cc-punc (_)
    (when (or (memq (char-before) +macim-chinese-punc-chars)
              (bolp))
      (delete-char 1)))
  (setq macim-inline-head-handler #'+macim-remove-head-space-after-cc-punc)

  (defun +macim-remove-tail-space-before-cc-punc (tighten-back-to)
    (when (> (point) tighten-back-to)
      (backward-delete-char (1- (- (point) tighten-back-to))))
    (when (and (eq (char-before) ? )
               (memq (char-after) +macim-chinese-punc-chars))
      (backward-delete-char 1)))
  (setq macim-inline-tail-handler #'+macim-remove-tail-space-before-cc-punc)

  ;; Before inserting Chinese punctuation, delete extra spaces introduced by inline mode
  (defun +macim-line-set-last-space-pos ()
    (when (eq (char-before) ?\s)
      (setq +macim-inline-english-last-space-pos (point))))
  (add-hook 'macim-inline-deactivated-hook #'+macim-line-set-last-space-pos)

  (defun +macim-inline-remove-redundant-space ()
    (when (eq +macim-inline-english-last-space-pos (1- (point)))
      (when (and (memq (char-before) +macim-chinese-punc-chars)
		 (eq (char-before (1- (point))) ?\s))
	(save-excursion
          (backward-char 2)
          (delete-char 1)
          (setq-local +macim-inline-english-last-space-pos nil)))
      (remove-hook 'post-self-insert-hook #'+macim-inline-remove-redundant-space t))
    )

  (defun +macim-inline-add-post-self-insert-hook ()
    (add-hook 'post-self-insert-hook #'+macim-inline-remove-redundant-space nil t))

  (add-hook 'macim-inline-deactivated-hook #'+macim-inline-add-post-self-insert-hook)
  )

(setopt ns-pop-up-frames nil)

(use-package winum
  :load-path "~/.emacs.d/packages/emacs-winum/"
  :hook (after-init . winum-mode)
  :custom
  (winum-auto-setup-mode-line nil))

(defun my/winum-select (num)
    (lambda (&optional arg) (interactive "P")
      (if arg
          (winum-select-window-by-number (- 0 num))
        (if (equal num (winum-get-number))
            (winum-select-window-by-number (winum-get-number (get-mru-window t)))
          (winum-select-window-by-number num)))))

(setq winum-keymap
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-0") 'winum-select-window-0-or-10)
        (dolist (num '(1 2 3 4 5 6 7 8 9) nil)
          (define-key map (kbd (concat "C-" (int-to-string num)))
                      (my/winum-select num)))
        map))

(with-eval-after-load 'window
  (setopt switch-to-buffer-in-dedicated-window 'pop
	  switch-to-buffer-obey-display-actions t))

(defun my/scroll-other-window-down ()
  "Scroll other window down."
  (interactive)
  (scroll-other-window-down 2))

(global-set-key (kbd "M-p") 'my/scroll-other-window-down)

(defun my/scroll-other-window ()
  "Scroll other window up."
  (interactive)
  (scroll-other-window 2))

(global-set-key (kbd "M-n") 'my/scroll-other-window)

(add-hook 'after-init-hook #'windmove-mode)
(with-eval-after-load 'windmove
  (windmove-default-keybindings))

(add-hook 'after-init-hook 'winner-mode)
(global-set-key (kbd "M-g u") #'winner-undo)
(global-set-key (kbd "M-g r") #'winner-redo)
(with-eval-after-load 'winner
  (setopt winner-dont-bind-my-keys t
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

(defun switch-to-message ()
  "Quick switch to `*Message*' buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(setopt message-kill-buffer-on-exit t
        message-kill-buffer-query nil
        message-sendmail-envelope-from 'header
        message-sendmail-extra-arguments '("-a" "outlook"))

(global-set-key (kbd "M-g m") #'switch-to-message)
(global-set-key (kbd "M-g s") #'scratch-buffer)

(global-set-key (kbd "C-x C-b") #'ibuffer)
;; (global-set-key (kbd "M-g p") 'previous-buffer)
;; (global-set-key (kbd "M-g n") 'next-buffer)

(add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode)
(with-eval-after-load 'ibuffer
  (setopt ibuffer-default-sorting-mode 'major-mode))

(use-package bufferlo
  :load-path "~/.emacs.d/packages/bufferlo/"
  :bind (([remap switch-to-buffer] . bufferlo-switch-to-buffer))
  :hook (after-init . bufferlo-mode))

(use-package helpful
  :load-path "~/.emacs.d/packages/helpful/"
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key))
  :config
  (add-to-list 'display-buffer-alist '("\\*helpful"
                                       (display-buffer-in-side-window)
                                       (side . right)
                                       (window-width . 0.5)
                                       (window-parameters
                                        (mode-line-format . none)))))

(use-package elisp-demos
  :load-path "~/.emacs.d/packages/elisp-demos/" "~/.emacs.d/packages/elisp-refs/"
  :after helpful
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;; 需要使用 C-c C-c 的地方不要使用 popper 管理，会导致按键不识别的问题.
(use-package popper
  :load-path "~/.emacs.d/packages/popper/"
  :bind (("C-`" . popper-toggle)
         :map popper-mode-map
         ("M-<tab>" . popper-cycle)
         ("M-`" . popper-toggle-type))
  :hook ((emacs-startup . popper-mode)
	 (popper-mode. popper-echo-mode))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$" "\\*Pp Eval Output\\*$"
          "\\*Compile-Log\\*"
          "\\*Completions\\*"
          "\\*Warnings\\*"
          "\\*Async Shell Command\\*"
          "\\*Apropos\\*"
          "\\*Backtrace\\*"
          "\\*Embark Actions\\*"
          "\\*Finder\\*"
          "\\*Kill Ring\\*"
          "\\*Go-Translate\\*"

          bookmark-bmenu-mode
          comint-mode
          compilation-mode
          help-mode helpful-mode
          tabulated-list-mode
          Buffer-menu-mode

          gnus-article-mode devdocs-mode
          grep-mode occur-mode rg-mode ag-mode pt-mode
          youdao-dictionary-mode osx-dictionary-mode fanyi-mode

          "^\\*Process List\\*" process-menu-mode
          list-environment-mode cargo-process-mode
          "^\\*EKG Capture"
          "^\\*Ibuffer\\*" ibuffer-mode
          "^\\*eshell.*\\*.*$" eshell-mode
          "^\\*shell.*\\*.*$"  shell-mode
          "^\\*terminal.*\\*.*$" term-mode
          "^\\*vterm.*\\*.*$"  vterm-mode
          "^\\*eldoc.*\\*.*$" eldoc-mode

          "\\*Flycheck errors\\*$" " \\*Flycheck checker\\*$"
          "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
          "\\*[Wo]*Man.*\\*$"
          "\\*ert\\*$" overseer-buffer-mode
          "\\*gud-debug\\*$"
          "\\*lsp-help\\*$" "\\*lsp session\\*$"
          "\\*quickrun\\*$"
	  "\\*One-Key\\*$"
          "\\*tldr\\*$"
          "\\*vc-.*\\*$"
          "^\\*elfeed-entry\\*$"
          "^\\*macro expansion\\**"

          "\\*TeX Help\\*"
          "^\\*denote-backlinks to "
          "\\*Agenda Commands\\*" "\\*Org Select\\*"))
  :config
  (setq popper-mode-line '(:eval (propertize "POP" 'face `(:inverse-video t))))
  ;; Enable indicator in minibuffer
  (defun my/popper--fit-window-height (win)
    "Determine the height of popup window WIN by fitting it to the buffer's content."
    (fit-window-to-buffer
     win
     (floor (frame-height) 2)
     (floor (frame-height) 3)))
  (setq popper-window-height #'my/popper--fit-window-height)
  ;; HACK: close popper with `C-g'
  (defun +popper-close-window-hack (&rest _)
    "Close popper window via `C-g'."
    (when (and (called-interactively-p 'interactive)
               (not (region-active-p))
               popper-open-popup-alist)
      (let ((window (caar popper-open-popup-alist)))
        (when (window-live-p window)
          (delete-window window)))))
  (advice-add #'keyboard-quit :before #'+popper-close-window-hack))

(when (and IS-MAC (executable-find "gls"))
  (setopt dired-use-ls-dired nil)
  (setopt insert-directory-program "gls")
  (setopt dired-listing-switches
	  "-l --almost-all --human-readable --group-directories-first --no-group"))

(add-hook 'dired-mode-hook (lambda ()
			     (setq-local truncate-lines t)))

(with-eval-after-load 'dired
  (setopt dired-dwim-target t
	  dired-auto-revert-buffer #'dired-buffer-stale-p
	  dired-recursive-copies 'always
	  dired-recursive-deletes 'top
	  dired-auto-revert-buffer t
	  dired-filename-display-length 'window))

(add-hook 'dired-mode-hook #'dired-omit-mode)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(with-eval-after-load 'dired
  (setopt dired-omit-verbose nil
	  dired-omit-files "^\\.[^.].*"))

(define-key dired-mode-map (kbd "s-.") #'dired-omit-mode)

(define-key dired-mode-map (kbd "C-c l") #'org-store-link)

(define-key dired-mode-map (kbd "C-c i") #'image-dired)

(use-package dired-preview
  :load-path "~/.emacs.d/packages/dired-preview/"
  :commands dired-preview-mode
  :custom
  (dired-preview-delay 0.0)
  (dired-preview-max-size (expt 2 20))
  (dired-preview-ignored-extensions-regexp
   (concat "\\."
	   "\\(mkv\\|webm\\|mp4\\|mp3\\|ogg\\|m4a"
	   "\\|gz\\|zst\\|tar\\|xz\\|rar\\|zip"
	   ;; "\\|png\\|jpg\\|jpeg"
	   "\\|iso\\|epub\\|pdf\\)")))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "P") #'dired-preview-mode))

(use-package dired-sidebar
  :load-path "~/.emacs.d/packages/dired-sidebar/"
  :bind ("C-x C-n" . dired-sidebar-toggle-sidebar)
  :custom
  (dired-sidebar-mode-line-format '("%e" my/winum "丨" mode-line-front-space mode-line-buffer-identification " " mode-line-end-spaces)))

;; Enhancing Dired Sorting With Transient
;; http://yummymelon.com/devnull/enhancing-dired-sorting-with-transient.html

(defun cc/--dired-sort-by (criteria &optional prefix-args)
  "Sort current Dired buffer according to CRITERIA and PREFIX-ARGS.

This function will invoke `dired-sort-other' with arguments built from
CRITERIA and PREFIX-ARGS.

CRITERIA is a keyword of which the following are supported:
  :name             :date-added             :version
  :kind             :date-metadata-changed  :size
  :date-last-opened :date-modified

PREFIX-ARGS is a list of GNU ls arguments. If nil, then it will use the value
of `cc-dired-listing-switches'. Otherwise this is typically populated by the
Transient menu `cc/dired-sort-by'.

This function requires GNU ls from coreutils installed.

See the man page `ls(1)' for details."
  (let ((arg-list (list "-l")))
    (if prefix-args
        (nconc arg-list prefix-args)
      (nconc arg-list cc-dired-listing-switches))
    (cond
     ((eq criteria :name)
      (message "Sorted by name"))

     ((eq criteria :kind)
      (message "Sorted by kind")
      (push "--sort=extension" arg-list))

     ((eq criteria :date-last-opened)
      (message "Sorted by date last opened")
      (push "--sort=time" arg-list)
      (push "--time=access" arg-list))

     ((eq criteria :date-added)
      (message "Sorted by date added")
      (push "--sort=time" arg-list)
      (push "--time=creation" arg-list))

     ((eq criteria :date-modified)
      (message "Sorted by date modified")
      (push "--sort=time" arg-list)
      (push "--time=modification" arg-list))

     ((eq criteria :date-metadata-changed)
      (message "Sorted by date metadata changed")
      (push "--sort=time" arg-list)
      (push "--time=status" arg-list))

     ((eq criteria :version)
      (message "Sorted by version")
      (push "--sort=version" arg-list))

     ((eq criteria :size)
      (message "Sorted by size")
      (push "-S" arg-list))

     (t
      (message "Default sorted by name")))

    (dired-sort-other (mapconcat 'identity arg-list " "))))

(unless (featurep 'transient)
  (require 'transient)
  (transient-define-prefix cc/dired-sort-by ()
    "Transient menu to sort Dired buffer by different criteria.

This function requires GNU ls from coreutils installed."
    :value '("--human-readable"
             "--group-directories-first"
             "--time-style=long-iso")
					; TODO: support cc-dired-listing-switches
    [["Arguments"
      ("-a" "all" "--all")
      ("g" "group directories first" "--group-directories-first")
      ("-r" "reverse" "--reverse")
      ("-h" "human readable" "--human-readable")
      ("t" "time style" "--time-style="
       :choices ("full-iso" "long-iso" "iso" "locale"))]

     ["Sort By"
      ("n"
       "Name"
       (lambda () (interactive)
	 (cc/--dired-sort-by :name
                             (transient-args transient-current-command)))
       :transient nil)
      ("k"
       "Kind"
       (lambda () (interactive)
	 (cc/--dired-sort-by :kind
                             (transient-args transient-current-command)))
       :transient nil)
      ("l"
       "Date Last Opened"
       (lambda () (interactive)
	 (cc/--dired-sort-by :date-last-opened
                             (transient-args transient-current-command)))
       :transient nil)
      ("a"
       "Date Added"
       (lambda () (interactive)
	 (cc/--dired-sort-by :date-added
                             (transient-args transient-current-command)))
       :transient nil)
      ("m"
       "Date Modified"
       (lambda () (interactive)
	 (cc/--dired-sort-by :date-modified
                             (transient-args transient-current-command)))
       :transient nil)
      ("M"
       "Date Metadata Changed"
       (lambda () (interactive)
	 (cc/--dired-sort-by :date-metadata-changed
                             (transient-args transient-current-command)))
       :transient nil)
      ("v"
       "Version"
       (lambda () (interactive)
	 (cc/--dired-sort-by :version
                             (transient-args transient-current-command)))
       :transient nil)
      ("s"
       "Size"
       (lambda () (interactive)
	 (cc/--dired-sort-by :size
                             (transient-args transient-current-command)))
       :transient nil)]]))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "s") #'cc/dired-sort-by))

(define-key dired-mode-map (kbd "M-g a") #'my/org-attach-visit-headline-from-dired)
(with-eval-after-load 'dired
  (defun my/org-attach-visit-headline-from-dired ()
    "Go to the headline corresponding to this org-attach directory."
    (interactive)
    (require 'org-attach)
    (let* ((path (replace-regexp-in-string (regexp-quote org-attach-directory) "" (expand-file-name (dired-filename-at-point))))
           (id-parts (split-string path "/"))
           (id1 (nth 1 id-parts))
           (id2 (nth 2 id-parts))
           (id (concat id1 id2)))
      (let ((m (org-id-find id 'marker)))
	(unless m (user-error "Cannot find entry with ID \"%s\"" id))
	(pop-to-buffer (marker-buffer m))
	(goto-char m)
	(move-marker m nil)
	(org-fold-show-context)))))

;; Prefer ripgrep, then ugrep, and fall back to regular grep.
(setopt xref-search-program (cond
			     ((or (executable-find "ripgrep")
				  (executable-find "rg"))
			      'ripgrep)
			     ((executable-find "ugrep")
			      'ugrep)
			     (t
			      'grep)))

(use-package rg
  :load-path "~/.emacs.d/packages/rg.el/"
  :bind ("M-s r" . rg)
  :custom
  (rg-group-result t)
  (rg-show-columns t))

(with-eval-after-load 'rg
  (add-to-list 'rg-finish-functions (lambda (buffer _) (pop-to-buffer buffer)))
  (add-to-list 'display-buffer-alist '("^\\*rg\\*"
                                       (display-buffer-in-side-window)
                                       (side . right)
                                       (window-width . 0.5))))

(setopt my/browser-engines
          '((DoubanMovie . "https://search.douban.com/movie/subject_search?search_text=")
            (DoubanBook . "https://search.douban.com/book/subject_search?search_text=")
            (Zhihu . "https://www.zhihu.com/search?type=content&q=")
            (Google . "https://www.google.com/search?q=")
            (Scholar . "https://scholar.google.com/scholar?q=")
            (SemanticScholar . "https://www.semanticscholar.org/search?q=")
            (Github . "https://github.com/search?q=")
            (Youtube . "http://www.youtube.com/results?aq=f&oq=&search_query=")
  	  (Bilibili . "https://search.bilibili.com/all?keyword=")
  	  (WikiPedia_en . "https://en.wikipedia.org/w/index.php?search=")
  	  (Annas-Archvie . "https://annas-archive.org/search?q=")))

  (defmacro my/define-search-functions ()
    "Dynamically define search functions for each search engine in `my/browser-engines`."
    `(progn
       ,@(mapcar (lambda (engine)
                   (let* ((engine-name (car engine))
                          (function-name (intern (format "my/search-%s" (downcase (symbol-name engine-name))))))
                     `(defun ,function-name (query)
                        ,(format "Search for QUERY using the %s engine." engine-name)
                        (interactive
                         (list (let ((default-query
                                      (if (and (eq system-type 'darwin)
                                               (featurep 'emt))
                                          (emt-word-at-point-or-forward)
                                        (thing-at-point 'word t))))
                                 (if (region-active-p)
                                     (buffer-substring-no-properties (region-beginning) (region-end))
                                   (read-string (format "[%s] Enter search terms (default: %s): " ,(symbol-name engine-name) default-query))))))
                        (let ((search-url (concat ,(cdr engine) (url-encode-url query))))
                          (browse-url search-url))
                        (when (region-active-p)
                          (deactivate-mark)))))
                 my/browser-engines)))

  (add-hook 'after-init-hook (lambda ()
  				 (my/define-search-functions)))

(global-set-key (kbd "M-s g") #'my/search-google)
(global-set-key (kbd "M-s W") #'my/search-wikipedia_en)
(global-set-key (kbd "M-s z") #'my/search-zhihu)
(global-set-key (kbd "M-s m") #'my/search-doubanmovie)
(global-set-key (kbd "M-s b") #'my/search-doubanbook)
;; (global-set-key (kbd "M-s y") #'my/search-youtube)
(global-set-key (kbd "M-s s") #'my/search-scholar)
(global-set-key (kbd "M-s S") #'my/search-semanticscholar)

(defun my/git-submodule-add (repo-url &optional dest-dir)
  "Add a git submodule with depth=1 into DEST-DIR.

REPO-URL is the Git repository URL.
DEST-DIR defaults to ~/.emacs.d/packages/."
  (interactive
   (list
    (read-string "Git repo URL: ")
    (read-directory-name
     "Destination directory: "
     (expand-file-name "packages/" user-emacs-directory))))

  (let* ((default-directory (file-name-as-directory user-emacs-directory))
         (repo-name (file-name-base (directory-file-name repo-url)))
         (target (expand-file-name repo-name dest-dir))
         (buffer (get-buffer-create "*git submodule add*")))
    (unless (file-directory-p dest-dir)
      (make-directory dest-dir t))

    (with-current-buffer buffer
      (erase-buffer))

    (let ((exit-code
           (process-file
            "git" nil buffer t
            "submodule" "add" "--depth=1"
            repo-url target)))
      (if (zerop exit-code)
          (message "Submodule added: %s" target)
        (progn
          (display-buffer buffer)
          (error "git submodule add failed"))))))

(use-package eee
  :load-path "~/.emacs.d/packages/eee.el/"
  :bind ("C-x g" . ee-lazygit)
  :custom
  (ee-terminal-command "/opt/homebrew/bin/wezterm"))

(with-eval-after-load 'eee
  (defun start-wezterm-at-current-directory ()
    "Start Wezterm at the current buffer's directory."
    (interactive)
    (let ((default-directory (or default-directory "~"))) ; 默认目录为当前 buffer 的目录
      (start-process "wezterm" nil "wezterm" "start" "--cwd" default-directory)))

  (defun switch-to-wezterm ()
    "Switch to WezTerm terminal."
    (interactive)
    (do-applescript "
    tell application \"WezTerm\"
      activate
    end tell"))

  (advice-add 'start-wezterm-at-current-directory :after
              (lambda (&rest _)
		(sleep-for 0.1)
		(switch-to-wezterm)))

  (advice-add 'ee-run :after
              (lambda (&rest _)
		(sleep-for 0.1)
		(switch-to-wezterm))))

(use-package browse-at-remote
  :load-path "~/.emacs.d/packages/browse-at-remote/"
  :bind ("M-g b" . browse-at-remote))

(use-package git-timemachine
  :load-path "~/.emacs.d/packages/git-timemachine/"
  :bind ("M-g t" . git-timemachine))

(defcustom prot-modeline-string-truncate-length 20
  "String length after which truncation should be done in small windows."
  :type 'natnum)

(defun prot-modeline--string-truncate-p (str)
  (and (< (window-total-width) split-width-threshold)
       (> (length str) prot-modeline-string-truncate-length)))

(defun prot-modeline-string-truncate (str)
  (if (prot-modeline--string-truncate-p str)
      (concat (substring str 0 prot-modeline-string-truncate-length) "...")
    str))

(defvar-local my/modeline-date
  '(:eval (when (and (mode-line-window-selected-p) (> (window-width) 90))
            (propertize (format-time-string " %Y-%m-%d %a ") 'face `(:inherit success)))))

(defvar-local my/modeline-time
    '(:eval (when (mode-line-window-selected-p)
              (propertize (format-time-string "%H:%M") 'face nil))))

(defvar-local my/modeline-timer
    '(:eval (when (and (mode-line-window-selected-p) (or org-timer-countdown-timer
							 pomm-current-mode-line-string))
              (propertize (my/modeline--timer) 'face `(:inherit font-lock-constant-face)))))

(defvar-local my/modeline-clock-info
    '(:eval (when (and (mode-line-window-selected-p) (org-clocking-p))
              (propertize (format " [%s](%s)"
                                  (org-duration-from-minutes
                                   (floor (org-time-convert-to-integer
                                           (org-time-since org-clock-start-time))
                                          60))
                                  org-clock-heading)
                          'face `(:inherit font-lock-builtin-face)))))

(defun my/modeline--major-mode ()
  (capitalize (string-replace "-mode" "" (symbol-name major-mode))))

(defvar-local my/modeline-major-mode
  '(:eval (propertize (my/modeline--major-mode) 'face `(:inherit font-lock-variable-name-face))))

(defun my/modeline-file-name ()
  (let ((name (or (buffer-file-name) (buffer-name))))
    (prot-modeline-string-truncate (file-name-nondirectory name))))

(defvar-local my/modeline-file-name
  '(:eval (propertize (my/modeline-file-name) 'face 'bold)))

;; 缓冲区状态指标
(defvar-local my/modeline-buffer-readonly
  '(:eval (when buffer-read-only (propertize " " 'face nil))))

(defvar-local my/modeline-buffer-modified
  '(:eval (propertize " * " 'face `(:inherit ,(if (buffer-modified-p) 'error nil)))))

;; 选区与位置信息
(defvar-local my/modeline-region-indicator
  '(:eval (when (and (mode-line-window-selected-p) (use-region-p))
            (propertize
             (concat " L" (number-to-string (count-lines (region-beginning) (region-end)))
                     " W" (number-to-string (count-words (region-beginning) (region-end)))
                     " C" (number-to-string (abs (- (mark t) (point)))) " ")))))

(defvar-local my/modeline-position
  '(:eval (when (mode-line-window-selected-p)
            (if (derived-mode-p 'pdf-view-mode)
                (propertize (format " %d/%d " (pdf-view-current-page) (pdf-cache-number-of-pages)) 'face font-lock-string-face)
              (propertize (format " %%l:%%c/%d " (line-number-at-pos (point-max))) 'face nil)))))

;; 电池状态 (内置 battery 增强)
(defun my/modeline--battery ()
  (let* ((data (and (functionp battery-status-function) (funcall battery-status-function)))
         (charging? (string-equal "AC" (cdr (assoc ?L data))))
         (percentage (car (read-from-string (or (cdr (assq ?p data)) "0")))))
    (if charging? (format "󱐋%d%%" percentage) (format "󰁹%d%%" percentage))))

(defvar-local my/modeline-battery
  '(:eval (when (mode-line-window-selected-p)
            (propertize (my/modeline--battery) 'face nil))))

(defun my/modeline--sys-coding-category ()
  (let ((sys (coding-system-plist buffer-file-coding-system)))
    (if (memq (plist-get sys :category)
              '(coding-category-undecided coding-category-utf-8))
        " UTF-8 "
      (upcase (symbol-name (plist-get sys :name))))))

(defun my/modeline--sys-coding-eol ()
  (let ((eol (coding-system-eol-type buffer-file-coding-system)))
    (pcase (coding-system-eol-type buffer-file-coding-system)
      (0 "LF")
      (1 "CRLF")
      (2 "CR")
      (_ ""))))

(defvar-local my/modeline-sys
  '(:eval (propertize (concat (my/modeline--sys-coding-category) (my/modeline--sys-coding-eol)) 'face nil)))

(defvar-local my/modeline-repeat
  '(:eval (when (and repeat-in-progress
                     (mode-line-window-selected-p))
            (propertize repeat-echo-mode-line-string 'face `(:inverse-video t)))))

(defun my/modeline--image-info ()
  (car (process-lines  "identify"  "-precision"  "3"  "-format"  "[%m %wx%h %[size]]" (buffer-file-name))))

(defvar-local my/modeline-image-info
    '(:eval (when (and (mode-line-window-selected-p) (or (eq major-mode 'image-mode)
                                                         (eq major-mode 'telega-image-mode)))
              (propertize (my/modeline--image-info) 'face font-lock-string-face))))

(defvar-local my/modeline-kbd-macro
    '(:eval
      (when (and (mode-line-window-selected-p) defining-kbd-macro)
        (propertize " KMacro " 'face `(:inherit font-lock-constant-face :inverse-video t)))))

(defvar-local my/winum
    '(:eval (when winum-mode
	      (propertize (format winum-format (winum-get-number-string)) 'face `(:inverse-video t )))))

(defvar-local my/modeline-gtd
  '(:eval (when (and org-gtd-mode (not (eq (org-gtd-inbox-count) 0)))
            (propertize (format " GTD[%d]" (org-gtd-inbox-count))
                        'face `(:inherit font-lock-builtin-face)))))

(dolist (construct '(my/modeline-major-mode
		     my/modeline-file-name
		     my/modeline-buffer-modified
		     my/modeline-kbd-macro
		     my/modeline-region-indicator
		     my/modeline-image-info
		     my/modeline-buffer-readonly
                     my/modeline-position
		     my/modeline-battery
		     my/modeline-sys
		     my/modeline-repeat
		     my/modeline-date
		     my/modeline-time
		     my/modeline-timer
		     my/modeline-clock-info
		     my/winum
		     my/modeline-gtd))

  (put construct 'risky-local-variable t))

(setopt mode-line-right-align-edge 'right-margin)

(setopt mode-line-format
        '("%e"
	  my/winum
	  (:eval (when repeat-mode
                   my/modeline-repeat))
          my/modeline-buffer-readonly
          my/modeline-buffer-modified
          my/modeline-file-name
          "  "
	  "%I"
	  my/modeline-kbd-macro
          my/modeline-position
	  my/modeline-image-info
          my/modeline-region-indicator
          mode-line-format-right-align  ; 关键：右对齐占位符
	  (:eval (with-eval-after-load 'org-clock
	   	   my/modeline-clock-info))
	  (:eval (when which-function-mode
		   which-func-format))
	  my/modeline-gtd
          my/modeline-sys
          " "
          my/modeline-major-mode
	  (project-mode-line project-mode-line-format)
	  (vc-mode vc-mode)
          " "))

(use-package keycast
  :load-path "~/.emacs.d/packages/keycast"
  :bind ("C-c i k" . keycast-mode-line-mode)
  :custom
  (keycast-mode-line-format "%2s%k%c%R")
  (keycast-mode-line-insert-after 'my/modeline-position)
  (keycast-mode-line-window-predicate 'mode-line-window-selected-p)
  (keycast-mode-line-remove-tail-elements nil))

(with-eval-after-load 'keycast-mode-line-mode
  (dolist (input '(self-insert-command org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))
  (dolist (event '(mouse-event-p mouse-movement-p mwheel-scroll))
    (add-to-list 'keycast-substitute-alist `(,event nil))))

(add-hook 'after-init-hook #'which-key-mode)
(with-eval-after-load 'which-key
  (setopt which-key-idle-delay 0.1))

(add-hook 'after-init-hook #'repeat-mode)
(with-eval-after-load 'repeat
  (setopt repeat-on-final-keystroke t
	  repeat-exit-timeout 5
	  repeat-exit-key "<escape>"
	  repeat-keep-prefix nil
	  repeat-check-key t
	  set-mark-command-repeat-pop t))

(add-hook 'emacs-startup-hook #'tab-bar-mode)
(global-set-key (kbd "s-t") #'tab-new)
(global-set-key (kbd "s-w") #'tab-close)
(with-eval-after-load 'tab-bar
  (setopt tab-bar-auto-width nil
	  tab-bar-new-tab-choice 'scratch-buffer
	  tab-bar-close-button-show nil
	  tab-bar-new-tab-to 'rightmost
	  tab-bar-separator "​"
	  tab-bar-select-tab-modifiers '(super)
	  tab-bar-tab-hints t
	  tab-bar-truncate t))

;; Mac 上需要先安装 tree-sitter 然后再编译 Emacs
;; brew install tree-sitter
(with-eval-after-load 'treesit
  (setopt treesit-language-source-alist
	  '((python "https://github.com/tree-sitter/tree-sitter-python.git")
	    (yaml "https://github.com/ikatyang/tree-sitter-yaml.git")
	    (typescript "https://github.com/tree-sitter/tree-sitter-typescript.git")
	    (json "https://github.com/tree-sitter/tree-sitter-json.git")
	    (csharp "https://github.com/tree-sitter/csharp-tree-sitter.git"))))

(add-hook 'LaTeX-mode-hook #'eglot-ensure)
(add-hook 'python-ts-mode #'eglot-ensure)
(add-hook 'csharp-ts-mode #'eglot-ensure)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(csharp-mode . ("~/Downloads/omnisharp-osx-arm64-net6/OmniSharp" "-lsp"))))

(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(use-package tempel
  :load-path "~/.emacs.d/packages/tempel/"
  :commands tempel-expand
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert)
	 (:map tempel-map
	       ("<down>" . tempel-next)))
  :init
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    ;; (setq-local corfu-auto-trigger "/"
    ;;             completion-at-point-functions
    ;;             (cons (cape-capf-trigger #'tempel-complete ?/)
    ;;                   completion-at-point-functions))
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  :custom
  (tempel-path `("~/.emacs.d/template/tempel"
                 ,(expand-file-name "config/tempel" my-galaxy))))

(use-package markdown-mode
  :load-path "~/.emacs.d/packages/markdown-mode/"
  :mode (("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
	      ("C-c C-e" . markdown-do)))

(add-to-list 'auto-mode-alist '("\\.yaml\\|\\.yml\\'" . yaml-ts-mode))

(use-package lua-mode
  :load-path "~/.emacs.d/packages/lua-mode/"
  :mode "\\.lua$"
  :interpreter "lua")

(global-set-key (kbd "C-<f5>") #'eshell)
(with-eval-after-load 'eshell
  (setopt eshell-directory-name (expand-file-name "eshell" cache-directory)))

(use-package vterm
  :load-path "~/.emacs.d/packages/emacs-libvterm/"
  :bind ("<f5>" . vterm)
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 5000))

(with-eval-after-load 'org
  (setopt org-ellipsis " ⇲"
	  org-modules '(org-habit)
	  org-imenu-depth 4
	  org-return-follows-link t
	  org-display-remote-inline-images 'download
	  org-log-into-drawer t
	  org-fast-tag-selection-single-key 'expert
	  org-adapt-indentation nil
	  org-fontify-quote-and-verse-blocks t
	  org-support-shift-select t
	  org-treat-S-cursor-todo-selection-as-state-change nil
	  org-hide-leading-stars nil
	  org-startup-indented nil
	  org-startup-with-inline-images t
	  org-image-actual-width nil
	  org-use-speed-commands t
	  org-highlight-latex-and-related '(latex script)
	  org-enforce-todo-dependencies t
	  org-enforce-todo-checkbox-dependencies t
	  org-export-allow-bind-keywords t
	  org-tags-sort-function 'org-string-collate-greaterp
	  org-lowest-priority ?D
	  org-priority-default ?C
	  org-columns-default-format "%50ITEM %TODO %3PRIORITY %TAGS"
	  org-persist-directory (expand-file-name "org-persist" cache-directory)))

(with-eval-after-load 'org-faces
  (setq org-todo-keyword-faces
        '(("TODO" . (:inherit (bold org-todo)))
          ("NEXT" . (:inherit (success org-todo)))
          ("CNCL" . (:inherit (shadow org-todo)))
          ("DONE" . (:inherit (button org-todo)))
          ("WAIT" . (:inherit (warning org-todo)))))
  (setq org-priority-faces
        '((?A . (bold . org-priority))
          (?B . org-priority)
          (?C . (shadow . org-priority)))))

(add-hook 'after-init-hook #'global-prettify-symbols-mode)
(setopt prettify-symbols-alist '(("lambda" . ?λ)
				 ("function" . ?𝑓)))

;; (defun my/org-prettify-symbols ()
;;   "利用内置 prettify-symbols-mode 美化 Org 标题符号"
;;   (push '("*" . ?①) prettify-symbols-alist)
;;   (push '("**" . ?②) prettify-symbols-alist)
;;   (push '("***" . ?③) prettify-symbols-alist)
;;   (push '("****" . ?④) prettify-symbols-alist)
;;   (push '("*****" . ?⑤) prettify-symbols-alist)
;;   (push '("******" . ?⑥) prettify-symbols-alist))

;; (add-hook 'org-mode-hook #'my/org-prettify-symbols)

(with-eval-after-load 'org-habit
  (setopt org-habit-graph-column 70))

;; ob-core
(with-eval-after-load 'ob-core
  (setq org-confirm-babel-evaluate nil)
  (defun my/org-babel-execute-src-block (&optional _arg info _params)
    "Load language if needed"
    (let* ((lang (nth 0 info))
           (sym (if (member (downcase lang) '("c" "cpp" "c++")) 'C (intern lang)))
           (backup-languages org-babel-load-languages))
      (unless (assoc sym backup-languages)
        (condition-case err
            (progn
              (org-babel-do-load-languages 'org-babel-load-languages (list (cons sym t)))
              (setq-default org-babel-load-languages (append (list (cons sym t)) backup-languages)))
          (file-missing
           (setq-default org-babel-load-languages backup-languages)
           err)))))
  (advice-add 'org-babel-execute-src-block :before 'my/org-babel-execute-src-block))

(add-to-list 'load-path "~/.emacs.d/packages/emacs-htmlize/")
(with-eval-after-load 'ox
  (require 'ox-html))

;; org-capture
(global-set-key (kbd "<f10>") #'org-capture)

(defun get-today-heading-with-subheading (subheading)
  (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward
               (concat "^\\* " (regexp-quote subheading))
               nil t)
        (goto-char (point-max))
        (insert "* " subheading "\n")))
  (list subheading))

(defun org-capture-heading-logs ()
  "Ensure a top-level * Notes :note: heading exists.
Return OLP for capture."
  (get-today-heading-with-subheading "Daily logs :Logs:"))

(defun org-capture-heading-notes ()
  "Ensure a top-level * Notes :note: heading exists.
Return OLP for capture."
  (get-today-heading-with-subheading "Fleet Notes :Note:"))

(with-eval-after-load 'org-capture
  (setq org-capture-templates
        `(("i" "Inbox"
           entry (file ,(concat icloud "iCloud~com~appsonthemove~beorg/Documents/org/inbox.org"))
           "* %?\n%U\n" :time-prompt t :tree-type week)
          ("l" "Inbox with link"
           entry (file ,(concat icloud "iCloud~com~appsonthemove~beorg/Documents/org/inbox.org"))
           "* %?\n %U\n%a\n" :time-prompt t :tree-type week)
	  ("f" "Fleet Note"
	   entry (file+function denote-journal-path-to-new-or-existing-entry org-capture-heading-notes)
	   "**  %?\n" :kill-buffer t)
	  ("d" "Daily Log"
	   entry (file+function denote-journal-path-to-new-or-existing-entry org-capture-heading-logs)
	   "**  %?\n" :kill-buffer t)
	  ("r" "Review"
           plain
           (file+olp+datetree ,(expand-file-name (format-time-string "logs/weekly_review_%Y.org") my-galaxy))
           (file "~/.emacs.d/template/review-weekly")
           :tree-type week :jump-to-captured t))))

(defun org-attach-save-file-list-to-property (dir)
    "Save list of attachments to ORG_ATTACH_FILES property."
    (when-let* ((files (org-attach-file-list dir)))
      (org-set-property "ORG_ATTACH_FILES" (mapconcat #'identity files ", "))))
(add-hook 'org-attach-after-change-hook #'org-attach-save-file-list-to-property)

(defun update-org-attach-property ()
  "Manually update the ORG_ATTACH_FILES property for the current Org entry."
  (interactive)
  (let* ((dir (org-attach-dir t))
         (files (org-attach-file-list dir)))
    (when (and dir files)
      (org-with-wide-buffer
       (org-set-property "ORG_ATTACH_FILES" (mapconcat #'identity files ", ")))
      (message "ORG_ATTACH_FILES property updated."))))

(with-eval-after-load 'org-attach
  (setopt org-attach-expert t
	  org-attach-id-dir (expand-file-name "attach" my-galaxy)
	  org-attach-id-to-path-function-list '(org-attach-id-ts-folder-format
						org-attach-id-uuid-folder-format)))

(with-eval-after-load 'org-id
  (setopt org-id-method 'ts
	  org-id-locations-file (expand-file-name ".org-id-locations" cache-directory)
	  org-id-link-to-org-use-id 'create-if-interactive))

(defun update-org-ids-in-directory (directory)
  "Update Org IDs in all Org files in DIRECTORY."
  (interactive "DEnter directory: ")
  (require 'org-id)
  (when (file-directory-p directory)
    (let ((org-files (directory-files-recursively directory "\\.org\\'")))
      (org-id-update-id-locations org-files t)
      (message "Updated Org IDs in %d files." (length org-files))))
  (unless (file-directory-p directory)
    (message "Not a valid directory: %s" directory)))

(with-eval-after-load 'org-src
  (setopt org-src-window-setup 'current-window
	  org-src-ask-before-returning-to-edit-buffer nil))

(use-package corg
  :load-path "~/.emacs.d/packages/corg.el/"
  :hook (org-mode . corg-setup))

;; org-goto
(with-eval-after-load 'org-goto
  (setq org-goto-interface 'outline-path-completion))

;; org-refile
(with-eval-after-load 'org-refile
  (setopt org-refile-targets '((nil :maxlevel . 9)
                               (org-agenda-files :maxlevel . 9))
	  org-refile-use-outline-path t
	  org-outline-path-complete-in-steps nil
	  org-refile-allow-creating-parent-nodes 'confirm
	  org-refile-use-outline-path 'file
	  org-refile-active-region-within-subtree t))

(with-eval-after-load 'org-clock
  (add-to-list 'warning-suppress-types
             '(files . "org-clock-save.el"))
  (org-clock-persistence-insinuate)
  (setopt org-clock-persist-file (expand-file-name "org-clock-save.el" cache-directory)
	  org-clock-history-length 23
	  org-clock-in-resume t
	  org-clock-into-drawer "LOGCLOCK"
	  org-clock-out-remove-zero-time-clocks t
	  org-clock-out-when-done t
	  org-clock-persist 'history
	  org-clock-clocktable-default-properties '(:maxlevel 5 :link t :tags t)
	  org-clock-persist-query-resume nil
	  org-clock-report-include-clocking-task t
	  org-clock-sound "/System/Library/Sounds/Ping.aiff")

  (add-hook 'org-after-todo-state-change-hook (lambda ()
                                                (if (org-clocking-p)
                                                    (org-clock-out)))))

(with-eval-after-load 'org-agenda
  (setopt org-agenda-window-setup 'other-tab
	  org-agenda-skip-scheduled-if-done t
	  org-agenda-skip-deadline-if-done t
	  org-agenda-todo-ignore-scheduled 'future
	  org-agenda-todo-ignore-deadlines 'near
	  org-agenda-dim-blocked-tasks t
	  org-agenda-compact-blocks t
	  org-agenda-align-tags-to-column 120
	  org-deadline-warning-days 7)
  (add-to-list 'org-agenda-custom-commands
	       '("b" "Book Shelf"
		 ((tags "+BookShelf"
			((org-agenda-prefix-format " %i")
			 (org-agenda-overriding-header "Reading Lists")))))))

(use-package grab-mac-link
  :load-path "~/.emacs.d/packages/grab-mac-link.el/"
  :commands grab-mac-link-dwim grab-mac-link-safari-1)

(defun my/link-grab ()
  (interactive)
  (grab-mac-link-dwim 'safari))

(global-set-key (kbd "M-g C-l") #'my/link-grab)

(defun my/copy-idlink ()
  "Copy idlink to clipboard."
  (interactive)
  (when (eq major-mode 'org-agenda-mode) ;switch to orgmode
    ;; (org-agenda-show)
    (org-agenda-goto))
  (when (eq major-mode 'org-mode) ; do this only in org-mode buffers
    (let* ((mytmphead (nth 4 (org-heading-components)))
           (mytmpid (funcall 'org-id-get-create))
           (mytmplink (format "[[id:%s][%s]]" mytmpid mytmphead)))
      (kill-new mytmplink)
      (message "Copied %s to killring (clipboard)" mytmplink))))

(defun jf/org-link-remove-link ()
  "Remove the link part of an `org-mode' link at point and keep only the description."
  (interactive)
  (let ((elem (org-element-context)))
    (when (eq (car elem) 'link)
      (let* ((content-begin (org-element-property :contents-begin elem))
             (content-end  (org-element-property :contents-end elem))
             (link-begin (org-element-property :begin elem))
             (link-end (org-element-property :end elem)))
        (when (and content-begin content-end)
          (let ((content (buffer-substring-no-properties content-begin content-end)))
            (delete-region link-begin link-end)
            (insert content)))))))

(use-package org-superstar
  :load-path "~/.emacs.d/packages/org-superstar-mode/"
  :hook ((org-mode . org-superstar-mode))
  :custom
  (org-hide-leading-stars t)
  (org-superstar-headline-bullets-list '("󰼏" "󰼐" "󰼑" "󰼒" "󰼓" "󰼔" "󰼕")))

(use-package form-feed
  :load-path "~/.emacs.d/packages/form-feed/"
  :hook ((org-mode . form-feed-mode)
	 (emacs-news-mode . form-feed-mode)))

(defun my/insert-specified-datetree ()
  "Insert a datetree entry for a specified date."
  (interactive)
  (let* ((date (org-parse-time-string (org-read-date)))
         (year (nth 5 date))
         (month (nth 4 date))
         (day (nth 3 date)))
    (org-datetree-find-date-create (list month day year))
    (open-line 1)
    (forward-line 1)))

(use-package denote
  :load-path "~/.emacs.d/packages/denote/" "~/.emacs.d/packages/denote-regexp/"
  :bind ((:map dired-mode-map
               ("r" . denote-dired-rename-marked-files-with-keywords)))
  :hook ((dired-mode . denote-dired-mode-in-directories)
	 (org-mode . denote-rename-buffer-mode))
  :custom
  (denote-rename-confirmations nil)
  (denote-org-store-link-to-heading nil)
  (denote-directory (expand-file-name "denote" my-galaxy))
  (denote-file-name-slug-functions '((title . denote-sluggify-title)
				     (signature . denote-sluggify-signature)
				     (keyword . identity)))
  ;; (denote-dired-directories
  ;;  (list denote-directory
  ;;        (thread-last denote-directory (expand-file-name "books"))
  ;;        (thread-last denote-directory (expand-file-name "outline"))
  ;;        (thread-last denote-directory (expand-file-name "literature"))
  ;;        (thread-last denote-directory (expand-file-name "term"))
  ;;        (thread-last denote-directory (expand-file-name "references"))))
  (denote-rename-buffer-format "%b %t")
  (denote-rename-buffer-backlinks-indicator ""))

(with-eval-after-load 'denote
  (defun my/modus-themes-denote-faces (&rest _)
    (modus-themes-with-colors
      (custom-set-faces
       `(denote-faces-year ((,c :foreground ,cyan)))
       `(denote-faces-month ((,c :foreground ,magenta-warmer)))
       `(denote-faces-day ((,c :foreground ,cyan)))
       `(denote-faces-time-delimiter ((,c :foreground ,fg-main)))
       `(denote-faces-hour ((,c :foreground ,magenta-warmer)))
       `(denote-faces-minute ((,c :foreground ,cyan)))
       `(denote-faces-second ((,c :foreground ,magenta-warmer))))))

  (add-hook 'ns-system-appearance-change-functions #'my/modus-themes-denote-faces))

(use-package denote-org
  :load-path "~/.emacs.d/packages/denote-org/"
  :after denote)

(use-package denote-journal
  :load-path "~/.emacs.d/packages/denote-journal/"
  :commands denote-journal-new-or-existing-entry
  :hook (calendar-mode . denote-journal-calendar-mode)
  :custom
  (denote-journal-directory
   (expand-file-name "journal" denote-directory))
  (denote-journal-keyword "journal"))

(defun my/get-weather ()
  (interactive)
  (shell-command "shortcuts run \"Get Current Weather\"")
  (do-applescript "tell application id \"org.gnu.Emacs\" to activate"))

(defun my/denote-journal-has-weather-p ()
  "Return non-nil if current buffer already contains a Weather entry."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^- Weather ::" nil t)))

(defun my/denote-journal-insert-weather-at-eof ()
  "Insert weather entry at end of journal file if missing."
  (unless (my/denote-journal-has-weather-p)
    (goto-char (point-max))
    (insert "\n- Weather :: ")))

(add-hook 'denote-journal-hook
          #'my/denote-journal-insert-weather-at-eof)

(use-package denote-search
  :load-path "~/.emacs.d/packages/denote-search/"
  :commands denote-search)

(use-package consult-denote
  :load-path "~/.emacs.d/packages/consult-denote/"
  :commands consult-denote-find consult-denote-grep
  :hook (org-mode . consult-denote-mode))

(use-package denote-explore
  :load-path "~/.emacs.d/packages/denote-explore/"
  :after denote
  :custom
  (denote-explore-network-filename (expand-file-name "mindmap/denote-network.html" my-galaxy))
  (denote-explore-json-edges-filename (expand-file-name "denote-edges.json" cache-directory))
  (denote-explore-json-vertices-filename (expand-file-name "denote-vertices.json" cache-directory)))

(defvar folder-structure-new
  '((:name "00_设计依据-方案-地勘" :subfolders ("01_设计说明" "02_甲方提供资料"))
    (:name "01_结构工程-施工图" :subfolders ("01_初设-提资" "02_初设-结构设计" "03_扩初-提资" "04_扩初-结构设计" "05_施工图-提资" "06_施工图_设计"))
    (:name "02_结构工程-计算模型" :subfolders ("01_初设模型" "02_扩初模型" "03_施工图模型"))
    (:name "03_结构工程-计算书" :subfolders ())
    (:name "04_审图" :subfolders ("01_审图意见" "02_审图修改"))
    (:name "05_施工配合" :subfolders ())
    (:name "06_图纸归档" :subfolders ("01_施工图_终版" "02_计算模型_终版" "02_计算书_终版"))
    (:name "07_参考资料" :subfolders ())
    )
  "预定义的文件夹树结构。")

(defvar folder-structure-reinforcement
  '((:name "00_检测鉴定报告" :subfolders ("01_测绘图纸" "02_检测鉴定报告" "03_现场照片"))
    (:name "01_加固设计文件" :subfolders ("01_方案设计" "02_初步设计" "03_施工图设计" "04_设计变更"))
    (:name "02_结构计算模型" :subfolders ("01_原结构模型" "02_加固方案模型" "03_最终模型"))
    (:name "03_计算书" :subfolders ("01_承载力验算" "02_抗震验算" "03_加固节点计算" "04_专家评审"))
    (:name "04_施工图文件" :subfolders ("01_施工图提资" "02_施工图_设计"))
    (:name "05_审图记录" :subfolders ("01_审图意见" "02_审图修改"))
    (:name "06_施工配合" :subfolders ())
    (:name "07_工程档案归档" :subfolders ("01_施工图" "02_加固模型" "03_计算书" "04_验收证书"))
    (:name "08_参考资料" :subfolders ()))
  "加固项目文件夹结构")

(defun create-folder-structure (base-path structure-type)
  "在指定路径下生成文件夹树结构。
BASE-PATH: 基础路径
STRUCTURE-TYPE: 结构类型，:new 或 :reinforcement"
  (let ((folder-structure (cond ((eq structure-type :new) folder-structure-new)
				((eq structure-type :reinforcement) folder-structure-reinforcement)
				(t (error "Invalid structure type: %s" structure-type)))))
    (dolist (folder folder-structure)
      (let* ((folder-name (plist-get folder :name))
             (subfolders (plist-get folder :subfolders))
             (parent-path (expand-file-name folder-name base-path)))
        (create-folder parent-path)
        (create-subfolders parent-path subfolders)))))

(defun create-folder (path)
  "创建指定路径的文件夹。"
  (unless (file-exists-p path)
    (make-directory path t)
    (message "创建文件夹: %s" path)))

(defun create-subfolders (parent-path subfolders)
  "在父文件夹路径下创建子文件夹。"
  (dolist (folder subfolders)
    (let ((subfolder-path (expand-file-name folder parent-path)))
      (create-folder subfolder-path))))

(defun generate-folder-tree ()
  "生成以日期和标题命名的文件夹，并在其中创建Readme文件。"
  (interactive)
  (let* ((current-date (format-time-string "%Y%m%d"))
         (title (read-string "请输入标题: "))
         ;; 使用 completing-read-multiple 输入多个 tag，用 _ 连接
         (tags (mapconcat #'identity
                          (completing-read-multiple
                           "请输入标签（多个标签用逗号或空格分隔）: "
                           nil nil nil nil nil nil)
                          "_"))
         (folder-name (concat current-date "==" title (if tags (concat "_" tags) "")))
         (base-path (expand-file-name folder-name "~/Te/"))
         ;; 选择文件夹结构类型
         (structure-type-string (completing-read "选择文件夹结构类型: "
                                                 '("new" "reinforcement")
                                                 nil t nil nil "new"))
         ;; 转换为关键字
         (structure-type (if (string= structure-type-string "new")
                             :new
                           :reinforcement)))

    ;; 创建文件夹
    (create-folder-structure base-path structure-type)

    ;; 创建Readme文件
    (with-temp-file (expand-file-name "Readme.txt" base-path)
      (insert (format "标题: %s\n标签: %s\n创建日期: %s\n结构类型: %s\n"
                      title
                      (if tags tags "无")
                      current-date
                      structure-type-string)))

    (message "文件夹 %s 创建完成，并生成了Readme文件！" folder-name)))

(use-package consult-notes
  :load-path "~/.emacs.d/packages/consult-notes/"
  :hook (org-mode . consult-notes-denote-mode)
  :custom
  (consult-notes-denote-files-function (lambda () (denote-directory-files nil t t)))
  ;; (consult-notes-file-dir-sources
  ;;  `(("Denote Notes"  ?d ,(expand-file-name "denote" my-galaxy))
  ;;    ("Book Reading"  ?b ,(expand-file-name "denote/books" my-galaxy))
  ;;    ("Meet"  ?m ,(expand-file-name "meeting" my-galaxy))
  ;;    ("References"  ?r ,(expand-file-name "denote/references" my-galaxy))
  ;;    ("Literature"  ?l ,(expand-file-name "denote/literature" my-galaxy))))
  )

(defvar my/dict-map (make-sparse-keymap)
  "Keymap for Dictionary commands.")

(global-set-key (kbd "C-c d") my/dict-map)

;; SDCV (本地词典 - 你的主力)
(define-key my/dict-map (kbd "d") #'my/search-dictionary)
(define-key my/dict-map (kbd "p") #'sdcv-search-pointer+)
(define-key my/dict-map (kbd "P") #'sdcv-search-pointer)
(define-key my/dict-map (kbd "i") #'sdcv-search-input+)
(define-key my/dict-map (kbd "I") #'sdcv-search-input)
(define-key my/dict-map (kbd "t") #'gt-translate)

;; Power Thesaurus (在线同义词)
(define-key my/dict-map (kbd "s") #'powerthesaurus-lookup-synonyms-dwim)
(define-key my/dict-map (kbd "a") #'powerthesaurus-lookup-antonyms-dwim)
(define-key my/dict-map (kbd "S") #'powerthesaurus-lookup-sentences-dwim)

;; 针对 Mac 的原生词典 (可选)
(when IS-MAC
  (define-key my/dict-map (kbd "m") #'osx-dictionary-search-pointer))

(use-package lexdb
  :load-path "~/.emacs.d/packages/lexdb/"
  :commands lexdb-search
  :config
  (require 'lexdb-ldoce)
  (setq lexdb-dictionaries
	'((:id ldoce
         :type ldoce
         :name "朗文当代"
         :db-file "~/.emacs.d/sdcv-dict/LDOCE6.db"
         :audio-dir "~/dicts/audio/"
         :priority 1)
	  ))
  (lexdb-init))

(use-package osx-dictionary
  :load-path "~/.emacs.d/packages/osx-dictionary.el/"
  :commands osx-dictionary-search-pointer osx-dictionary-search-input)

(use-package sdcv
  :load-path "~/.emacs.d/packages/sdcv/"
  :commands sdcv-search-pointer sdcv-search-pointer+ sdcv-search-input sdcv-search-input+
  :config
  (defun my/search-dictionary (arg)
    (interactive "P")
    (if arg
        (sdcv-search-pointer)
      (sdcv-search-pointer+)))
  (defun my/sdcv-tooltip-face-toggle ()
    (interactive)
    (let* ((theme-name (symbol-name (car custom-enabled-themes)))
           (palette-var (intern (concat theme-name "-palette")))
           (fg-value (cadr (assoc 'fg-main (eval palette-var))))
           (bg-value (cadr (assoc 'bg-main (eval palette-var))))
           (face-spec `((((background light))
                         :foreground ,fg-value :background ,bg-value)
                        (t
                         :foreground ,fg-value :background ,bg-value))))
      (face-spec-set 'sdcv-tooltip-face face-spec 'face-override-spec)))
  (advice-add 'sdcv-search-pointer+ :before #'my/sdcv-tooltip-face-toggle)
  (advice-add 'sdcv-search-input+ :before #'my/sdcv-tooltip-face-toggle)
  (setq sdcv-tooltip-border-width 1)
  (setq sdcv-dictionary-data-dir (expand-file-name "sdcv-dict" user-emacs-directory))
  (setq sdcv-program "/opt/homebrew/bin/sdcv")
  (setq sdcv-dictionary-simple-list    ;星际译王屏幕取词词典, 简单, 快速
        '("懒虫简明英汉词典"
          "懒虫简明汉英词典"
          "KDic11万英汉词典"))
  (setq sdcv-dictionary-complete-list     ;星际译王的词典, 完全, 详细
        '("牛津英汉双解美化版"
          "懒虫简明英汉词典"
          "英汉汉英专业词典"
          "XDICT英汉辞典"
          "stardict1.3英汉辞典"
          "WordNet"
          "XDICT汉英辞典"
          "Jargon"
          "懒虫简明汉英词典"
          "FOLDOC"
          "新世纪英汉科技大词典"
          "KDic11万英汉词典"
          "朗道汉英字典5.0"
          "CDICT5英汉辞典"
          "新世纪汉英科技大词典"
          "21世纪双语科技词典"
          "quick_eng-zh_CN")))

(use-package powerthesaurus
  :load-path "~/.emacs.d/packages/emacs-powerthesaurus/"
  :commands (powerthesaurus-lookup-synonyms-dwim
	     powerthesaurus-lookup-antonyms-dwim
	     powerthesaurus-lookup-related-dwim
	     powerthesaurus-lookup-definitions-dwim
	     powerthesaurus-lookup-sentences-dwim))

(use-package jinx
  :load-path "~/.emacs.d/packages/jinx/"
  :hook (org-mode . jinx-mode)
  :bind ("M-#" . jinx-correct)
  :config
  (add-to-list 'jinx-exclude-regexps '(t "\\cc")))

(use-package gt
  :load-path "~/.emacs.d/packages/go-translate/"
  :commands gt-translate
  :config
  (add-to-list 'display-buffer-alist '("^\\*gt-result\\*"
                                       (display-buffer-in-side-window)
                                       (side . bottom)
                                       (height . 0.3)))
  (setq gt-buffer-render-follow-p t)
  (setq gt-langs '("en" "zh"))
  (setq gt-buffer-render-window-config
        '((display-buffer-reuse-window display-buffer-in-direction)
          (direction . bottom)
          (window-height . 0.4)))
  (setq gt-pop-posframe-forecolor (face-foreground 'tooltip nil t)
        gt-pop-posframe-backcolor (face-background 'tooltip nil t))
  (when (facep 'posframe-border)
    (setq gt-pin-posframe-bdcolor (face-background 'posframe-border nil t)))

  (setq gt-default-translator (gt-translator :engines (gt-google-engine :cache 'word)
					     :render (list (gt-posframe-pop-render :if 'word :frame-params (list :border-width 0 :border-color "red"))
							   (gt-buffer-render :then (gt-kill-ring-render))))))

(setopt my/reference-lists `(,(concat my-galaxy "/bibtexs/My Library.bib")
                             ,(concat my-galaxy "/bibtexs/Books.bib")
			     ,(concat my-galaxy "/bibtexs/Seismic.bib")))
(use-package bibtex
  :mode ("\\.bib\\'" . bibtex-mode)
  :custom
  (bibtex-align-at-equal-sign t)
  (bibtex-autokey-year-length 4)
  (bibtex-autokey-name-year-separator "-")
  (bibtex-autokey-year-title-separator "-")
  (bibtex-autokey-titleword-separator "-")
  (bibtex-autokey-titlewords 2)
  (bibtex-autokey-titlewords-stretch 1)
  (bibtex-autokey-titleword-length 5))

(use-package reftex
  :hook (LaTeX-mode . turn-on-reftex)
  :bind ([remap reftex-citation] . citar-insert-citation)
  :custom
  (reftex-toc-follow-mode t)
  (reftex-toc-split-windows-horizontally t)
  (reftex-toc-split-windows-fraction 0.25))

(with-eval-after-load 'oc
  (setq org-cite-global-bibliography my/reference-lists))

(with-eval-after-load 'org
  (with-eval-after-load 'oc
    (define-key org-mode-map [remap org-cite-insert] #'citar-insert-citation)
    (with-eval-after-load 'citar
      (require 'citar-org)
      (setq org-cite-insert-processor 'citar)
      (setq org-cite-follow-processor 'citar)
      (setq org-cite-activate-processor 'citar))))

(use-package citar
  :load-path "~/.emacs.d/packages/citar/" "~/.emacs.d/packages/parsebib/"
  :commands citar-create-note
  :custom
  (citar-templates '((main . "${=type=:12}|${date year issued:4}| ${title:80}")
		     (suffix . " |${=key= id} |${tags keywords:*} |${author editor:20%sn}") ;
		     (preview . "${author editor:%etal} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.")
		     (note . "Notes on ${author editor:%etal}, ${title}")))
  (citar-indicators (list citar-indicator-links
                          citar-indicator-files
                          citar-indicator-notes
                          citar-indicator-cited))
  (citar-bibliography my/reference-lists)
  (citar-library-paths `(,(expand-file-name "PDF/" my-galaxy)))
  (citar-notes-paths `(,(expand-file-name "denote/references" my-galaxy)))
  (citar-library-file-extensions '("pdf" "jpg" "epub"))
  (citar-symbol-separator "​")
  (citar-select-multiple nil)
  (citar-file-additional-files-separator "-")
  (citar-at-point-function 'embark-act))

(with-eval-after-load 'citar
  (unless (featurep 'citar-capf)
    (require 'citar-capf))
  (add-hook 'LaTex-mode-hook #'citar-capf-setup)
  (add-hook 'org-mode-hook #'citar-capf-setup))

(with-eval-after-load 'citar-org
  (define-key citar-org-citation-map (kbd "RET") 'org-open-at-point))

(use-package citar-embark
  :load-path "~/.emacs.d/packages/citar/"
  :after citar
  :hook (org-mode . citar-embark-mode))

(use-package citar-denote
  :load-path "~/.emacs.d/packages/citar-denote/"
  :hook (org-mode . citar-denote-mode)
  :custom
  (citar-denote-use-bib-keywords t)
  (citar-denote-subdir t))

(use-package biblio
  :load-path "~/.emacs.d/packages/biblio.el/"
  :commands biblio-lookup biblio-crossref-lookup)

(use-package scihub
  :load-path "~/.emacs.d/packages/scihub.el/"
  :commands scihub
  :config
  (setq scihub-download-directory "~/Downloads/"
	scihub-open-after-download t
	scihub-fetch-domain 'scihub-fetch-domains-lovescihub))

;; Need install bibutils.
;; https://sourceforge.net/p/bibutils/home/Bibutils/
;; brew install bibutils
;;;###autoload
(defun my/bib2end (bib-file end-file)
  "Convert BibTeX file to EndNote file."
  (interactive
   (list (read-file-name "BibTeX File: "
			 (expand-file-name "bibtexs/" my-galaxy) nil nil ".bib")
         (read-file-name "Output EndNote File: "
			 (expand-file-name "bibtexs/" my-galaxy) nil nil ".end")))
  (let* ((xml-file (make-temp-file "bib2xml" nil ".xml"))
         (bib2xml-cmd (format "bib2xml %s > %s" bib-file xml-file))
         (xml2end-cmd (format "xml2end %s > %s" xml-file end-file)))
    (when (= 0 (shell-command bib2xml-cmd))
      (shell-command xml2end-cmd)
      (message "XML to EndNote conversion successful."))
    (delete-file xml-file)))

(with-eval-after-load 'oc-biblatex
  (setopt org-cite-biblatex-styles
          '((nil nil "cite" nil nil))))

;; ox-latex 是 Emacs 中 Org-mode 导出框架中的一个子模块
;; minted 需要安装 Pygments, brew install pygments
(with-eval-after-load 'ox-latex
  (setopt org-latex-compiler "xelatex")
  (setopt org-latex-prefer-user-labels t)
  (setopt org-latex-src-block-backend 'minted) ;file name should not contain space.
  ;; (setopt org-latex-minted-options '(("linenos")
  ;; 				     ("numbersep" "5pt")
  ;; 				     ("frame"     "none") ; box frame is created by `mdframed' package
  ;; 				     ("framesep"  "2mm")
  ;; 				     ("breaklines")))
  (setopt org-latex-pdf-process '("xelatex -shell-escape %f"
				  "biber %b"
				  "xelatex -shell-escape %f"
				  "xelatex -shell-escape %f"
				  "rm -f %b.out %b.log %b.tex %b.brf %b.bbl %b.bcf %b.run.xml"))
  (setopt org-latex-logfiles-extensions '("lof" "lot" "tex~" "aux" "idx" "log"
					  "out" "toc" "nav" "snm" "vrb" "dvi"
					  "fdb_latexmk" "blg" "brf" "fls"
					  "entoc" "ps" "spl" "bbl"))
  (setopt org-latex-hyperref-template  (concat "\\hypersetup{\n"
                                               "pdfauthor={%a},\n"
                                               "pdftitle={%t},\n"
                                               "pdfkeywords={%k},\n"
                                               "pdfsubject={%d},\n"
                                               "pdfcreator={%c},\n"
                                               "pdflang={%L},\n"
                                               "colorlinks,\n"
                                               "citecolor=gray,\n"
                                               "filecolor=gray,\n"
                                               "linkcolor=gray,\n"
                                               "urlcolor=gray\n"
                                               "}\n"))
  (setopt org-latex-classes nil)
  (add-to-list 'org-latex-classes
               '("book"
                 "\\documentclass[UTF8,twoside,a4paper,12pt,openright]{ctexrep}
                   [NO-DEFAULT-PACKAGES]
                   [NO-PACKAGES]
                   [EXTRA]"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("article-cn"
                 "\\documentclass{ctexart}
                  [NO-DEFAULT-PACKAGES]
                  [NO-PACKAGES]
                  [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("article"
                 "\\documentclass[11pt]{article}
                 [NO-DEFAULT-PACKAGES]
                 [NO-PACKAGES]
                 [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("beamer"
                 "\\documentclass[presentation]{beamer}
                 [DEFAULT-PACKAGES]
                 [PACKAGES]
                 [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(use-package tex
  :load-path "~/.emacs.d/packages/auctex/"
  :init
  (load "auctex-autoloads.el" nil t t)
  :mode ("\\.tex\\'" . latex-mode)
  :custom
  (TeX-PDF-mode t)
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-save-query nil)
  (TeX-master 'dwim)
  (TeX-electric-sub-and-superscript t)
  (TeX-auto-local (expand-file-name ".auctex-auto" cache-directory))
  (TeX-style-local (expand-file-name ".auctex-style" cache-directory))
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-method '((dvi . source-specials) (pdf . synctex)))
  (TeX-source-correlate-start-server t))

(with-eval-after-load 'tex              ;auctex
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex -shell-escape --synctex=1%(mode)%' %t" TeX-run-TeX nil t))
  (add-to-list 'TeX-view-program-list-builtin '("PDF Tools" TeX-pdf-tools-sync-view))
  (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(with-eval-after-load 'latex
  (define-key LaTeX-mode-map (kbd "C-c h .") #'TeX-doc))

(use-package auctex-latexmk
  :load-path "~/.emacs.d/packages/auctex-latexmk/"
  :hook (LaTeX-mode . auctex-latexmk-setup))

(defun org-export-docx (input csl)
  (interactive "FInput file (Default is Buffer File):\nFCSL file (Default is chinese-gb7714-2005-numeric):")
  (let* ((base (or (file-name-sans-extension input) (buffer-file-name)))
	 (csl (or (expand-file-name "csl/chinese-gb7714-2005-numeric.csl" user-emacs-directory)))
	 (output (concat base ".docx")))
    (shell-command (format "pandoc %s -o %s --citeproc --csl %s" input output csl))))

(use-package pdf-tools
  :load-path ("packages/pdf-tools/lisp" "packages/tablist")
  :commands pdf-tools-install
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install t nil t nil))

(use-package pdf-view
  :hook ((pdf-tools-enabled . pdf-view-themed-minor-mode)
         (pdf-view-mode . (lambda ()
                           (require 'saveplace-pdf-view))))
  :config
  (setq pdf-view-display-size 'fit-width)
  (setq pdf-view-use-unicode-ligther nil)
  (setq pdf-view-use-scaling t)
  (setq pdf-view-use-imagemagick nil)
  (setq pdf-annot-activate-created-annotations nil))

(use-package pdf-roll
  :hook (pdf-view-mode . pdf-view-roll-minor-mode))

(use-package pdf-occur
  :hook (pdf-view-mode . pdf-occur-global-minor-mode))

(use-package pdf-history
  :hook (pdf-view-mode . pdf-history-minor-mode))

(use-package pdf-links
  :hook (pdf-view-mode . pdf-links-minor-mode))

(use-package pdf-outline
  :hook (pdf-view-mode . pdf-outline-minor-mode)
  :bind (:map pdf-outline-buffer-mode-map
              ("RET" . pdf-outline-follow-link-and-quit)))

(use-package pdf-annot
  :hook (pdf-view-mode . pdf-annot-minor-mode)
  :bind (:map pdf-annot-edit-contents-minor-mode-map
              ("<return>" . pdf-annot-edit-contents-commit)
              ("<S-return>" . newline)))

(use-package pdf-sync
  :hook (pdf-view-mode . pdf-sync-minor-mode))

(use-package pdf-cache
  :after pdf-view
  :config
  (define-pdf-cache-function pagelabels))

(use-package pdf-misc
  :after pdf-view
  :config
  (setq pdf-misc-print-program-executable "/usr/bin/lp")
  (defun mrb/pdf-misc-print-pages(filename pages &optional interactive-p)
    "Wrapper for `pdf-misc-print-document` to add page selection support."
    (interactive (list (pdf-view-buffer-file-name)
                       (read-string "Page range (empty for all pages): "
                                    (number-to-string (pdf-view-current-page)))
                       t) pdf-view-mode)
    (let ((pdf-misc-print-program-args
           (if (not (string-blank-p pages))
               (cons (concat "-P " pages) pdf-misc-print-program-args)
             pdf-misc-print-program-args)))
      (pdf-misc-print-document filename))))

(with-eval-after-load 'pdf-view
  (define-key pdf-view-mode-map [remap pdf-misc-print-document] #'mrb/pdf-misc-print-pages))

(use-package saveplace-pdf-view
  :load-path "~/.emacs.d/packages/saveplace-pdf-view/"
  :defer t)

;; https://gist.github.com/krisbalintona/f4554bb8e53c27c246ae5e3c4ff9b342
;;;###autoload
(defun krisb-pdf-tools-metadata-bookmark-section ()
  "Insert bookmark metadata section."
  (interactive)
  (save-excursion
    (insert "\nBookmarkBegin\nBookmarkTitle: \nBookmarkLevel: 1\nBookmarkPageNumber: "))
  (move-end-of-line 2))

(defvar-keymap krisb-pdf-tools-metadata-mode-map
  :doc "Mode map for `krisb-pdf-tools-metadata-mode'."
  "C-c C-b" #'krisb-pdf-tools-metadata-bookmark-section)

(define-derived-mode krisb-pdf-tools-metadata-mode fundamental-mode "Metadata"
  "Major mode for altering and viewing PDF metadata."
  :interactive t
  (use-local-map krisb-pdf-tools-metadata-mode-map))

;;;###autoload
(defun krisb-pdf-tools-metadata-modify (pdf-file)
  "Modify PDF-FILE metadata."
  (interactive (list (buffer-file-name)))
  (unless (string= "pdf" (file-name-extension pdf-file))
    (user-error "File is not a PDF!"))
  (unless (executable-find "pdftk")
    (error "System executable `pdftk' not found. Please install executable on filesystem to proceed"))
  (let* ((pdf-name (file-name-sans-extension (file-name-nondirectory pdf-file)))
         (buf-name (concat "*pdf-tools metadata: " pdf-name))
         (metadata-file (concat "/tmp/pdf-tools-metadata--" pdf-name))
         (temp-pdf (make-temp-file "/tmp/pdf-tools-metadata--temp-pdf"))
         (metadata-dump-command (concat "pdftk \"" pdf-file "\" dump_data"))
         (metadata-update-command
          (concat "pdftk \"" pdf-file "\" update_info \"" metadata-file "\" output \"" temp-pdf "\""))
         (commit-func (lambda ()
                        "Commit the changes to PDF metadata."
                        (interactive)
                        (with-current-buffer buf-name
                          (widen)
                          (write-region (point-min) (point-max) metadata-file))
                        (shell-command metadata-update-command "*pdf-tools metadata: CLI output")
                        (kill-buffer buf-name)
                        ;; Have to do it this way since `pdftk' does not allow
                        ;; having the output file be the input file
                        (rename-file temp-pdf pdf-file t)
                        (message "Updated metadata!"))))
    (save-buffer)
    (with-current-buffer (get-buffer-create buf-name)
      (insert (shell-command-to-string metadata-dump-command))
      (goto-char (point-min))
      (krisb-pdf-tools-metadata-mode))
    (pop-to-buffer buf-name)
    (define-key krisb-pdf-tools-metadata-mode-map (kbd "C-c C-c") commit-func)
    (set-buffer-modified-p nil)
    (message (substitute-command-keys "Press `C-c C-c' when finished editing PDF metadata. To see keybinds, press \\[describe-mode]"))))

(use-package nov
  :load-path "~/.emacs.d/packages/nov.el/" "~/.emacs.d/packages/esxml"
  :mode (".epub" . nov-mode)
  :custom
  (nov-unzip-program (executable-find "bsdtar"))
  (nov-unzip-args '("-xC" directory "-f" filename))
  (nov-save-place-file (expand-file-name "nov_place" cache-directory)))

(use-package ibooks-annot
  :load-path "~/.emacs.d/packages/ibooks-annot.el/"
  :commands ibooks-annot/extract-annotations-to-note ibooks-annot/open-book-with-ibooks
  :custom
  (pdfannots-script "~/.emacs.d/packages/pdfannots/pdfannots.py -f json")
  (ibooks-annot/book-note-directory (expand-file-name "denote/books" my-galaxy)))

(defvar my/publish-directory "~/Blogs/")

(with-eval-after-load 'ox-publish
  (setq org-publish-project-alist `(("site"
				     :base-directory ,website-directory
				     :base-extension "org"
				     :recursive nil
				     :publishing-directory ,my/publish-directory
				     :publishing-function org-html-publish-to-html)

				    ("posts"
				     :base-directory ,(expand-file-name "posts" website-directory)
				     :base-extension "org"
				     :publishing-directory ,(expand-file-name "posts" my/publish-directory)
				     :publishing-function org-html-publish-to-html
				     :with-author t
				     :auto-sitemap t
				     :sitemap-filename "index.org"
				     :sitemap-title "posts"
				     :sitemap-sort-files anti-chronologically
				     :sitemap-format-entry taingram--sitemap-dated-entry-format)

				    ("static"
				     :base-directory ,website-directory
				     :base-extension "css\\|js\\|txt\\|jpg\\|gif\\|png"
				     :recursive t
				     :publishing-directory  ,my/publish-directory
				     :publishing-function org-publish-attachment)

				    ("personal-website" :components ("site" "posts" "static"))))

  ;; https://git.sr.ht/~taingram/taingram.org/tree/master/item/publish.el
  (defun taingram--sitemap-dated-entry-format (entry style project)
    "Sitemap PROJECT ENTRY STYLE format that includes date."
    (let ((filename (org-publish-find-title entry project)))
      (if (= (length filename) 0)
          (format "*%s*" entry)
        (format "{{{timestamp(%s)}}}   [[file:%s][%s]]"
                (format-time-string "%Y-%m-%d"
                                    (org-publish-find-date entry project))
                entry
                filename))))

  (defun my/ox-publish-move-images (origin publish)
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[file:\\(.*?\\)\\]\\]" nil t)
        (let* ((image-path (match-string 1))
               (picture-name (car (last (split-string image-path "/"))))
               (new-path (concat my/publish-directory "static/" picture-name)))
          (copy-file image-path new-path t)))))

  (defun my/ox-publish-replace-src-path (origin publish)
    "Replace image paths in the HTML file."
    (interactive)
    (message "%s%s" origin publish)
    (with-temp-buffer
      (insert-file-contents publish)
      (goto-char (point-min))
      (while (re-search-forward (concat "file://" (expand-file-name my-galaxy) "/pictures/") nil t)
        (replace-match "../static/"))
      (write-region (point-min) (point-max) publish)))
  (add-hook 'org-publish-after-publishing-hook 'my/ox-publish-move-images)
  (add-hook 'org-publish-after-publishing-hook 'my/ox-publish-replace-src-path))

(with-eval-after-load 'ox-html
  (setq org-export-global-macros
        '(("timestamp" . "@@html:<span class=\"timestamp\">[$1]</span>@@")))
  (setq org-html-preamble t)
  (setq org-html-preamble-format
	'(("en" "<a href=\"/index.html\" class=\"button\">Home</a>
               <a href=\"/posts/index.html\" class=\"button\">Posts</a>
               <a href=\"/about.html\" class=\"button\">About</a>
               <hr>")))

  (setq org-html-postamble t)

  (setq org-html-postamble-format
        '(("en" "<hr><div class=\"info\"> <span class=\"created\">Created with %c on MacOS</span>
 <span class=\"updated\">Updated: %d</span> </div>")))

  (setq org-html-head-include-default-style nil)

  (setq org-html-head
        "<link rel=\"stylesheet\" type=\"text/css\" href=\"/css/style.css\" />
         <script src=\"js/copy.js\"></script> "))

(defun blog-git-auto-push ()
  "Automatically stage, commit, and push changes to the blog repository."
  (interactive)
  (let* ((blog-dir "~/blog/") ; 替换为你的博客目录
         (commit-message (format-time-string "Blog update %Y-%m-%d %H:%M:%S")))
    (save-some-buffers t) ; 保存所有未保存的缓冲区
    (shell-command (concat "cd " blog-dir " && git add ."))
    (shell-command (concat "cd " blog-dir " && git commit -m \"" commit-message "\""))
    (shell-command (concat "cd " blog-dir " && git push origin main"))
    (message "Blog changes staged, committed, and pushed!")))

(transient-define-prefix my/knowledge-menu ()
  "Knowledge Management"
  [["Denote Create"
    ("n" "New Note" denote)
    ("t" "Template" denote-template)
    ("d" "Date/Journal" denote-date)
    ("s" "Subdirectory" denote-subdirectory)]
   ["Link & Connect"
    ("i" "Insert Link" denote-link)
    ("I" "Batch Links" denote-add-links)
    ("b" "Backlinks" denote-backlinks)
    ("L" "Create & Link" denote-link-after-creating)]
   ["Denote Manage"
    ("r" "Rename File" denote-rename-file)
    ("R" "Rename using Front Matter" denote-rename-file-using-front-matter)
    ("k" "Keywords (Add/Remove)" denote-keywords-add)
    ]
   ["References"
    ("C-n" "Create Ref Note" citar-create-note)
    ("o" "Open Ref file" citar-open-files)
    ("N" "Open Ref note" citar-open-notes)
    ("2" "Bib to Endnote" my/bib2end)
    ]
   ["Annot&Blog"
    ("a" "Extract" ibooks-annot/extract-annotations-to-note)
    ("p" "Blog Push" blog-git-auto-push)
    ]
   ["Search"
    ("f" "Consult Find" consult-notes)
    ("F" "Find" consult-denote-find)
    ("g" "Grep" consult-denote-grep)
    ]
   ["Diary"
    ("j" "Journal" denote-journal-new-or-existing-entry)
    ]])

;; 绑定到 C-c n m (Menu)
(global-set-key (kbd "C-c n") 'my/knowledge-menu)

(add-hook 'calendar-today-visible-hook #'calendar-mark-today)
(add-hook 'diary-mode-hook #'appt-activate)
(add-hook 'diary-list-entries-hook #'diary-sort-entries)
(add-hook 'diary-mode-hook #'goto-address-mode)

(global-set-key (kbd "C-x c") #'calendar)
(with-eval-after-load 'calendar
  (setopt calendar-view-diary-initially-flag t
	  calendar-mark-diary-entries-flag t
	  calendar-date-style 'iso
	  calendar-date-display-form calendar-iso-date-display-form
	  diary-date-forms diary-iso-date-forms
	  calendar-time-display-form '(24-hours ":" minutes
						(when time-zone
						  (format "(%s)" time-zone)))))
(with-eval-after-load 'appt
  (setopt appt-display-diary nil
	  appt-disp-window-function #'appt-disp-window
	  appt-display-mode-line t
	  appt-display-interval 3
	  appt-audible nil
	  appt-warning-time-regexp "appt \\([0-9]+\\)"
	  appt-message-warning-time 6))

(with-eval-after-load 'diary-lib
  (setopt diary-display-function #'diary-fancy-display
	  diary-header-line-format nil
	  diary-list-include-blanks nil
	  diary-abbreviated-year-flag nil
	  diary-number-of-entries 7
	  diary-comment-start ");;"
	  diary-comment-end ""
	  diary-nonmarking-symbol "!"
	  diary-file (expand-file-name "logs/diary.org" my-galaxy)))

(use-package beancount
  :load-path "~/.emacs.d/packages/beancount-mode/"
  :mode ("\\.bean\\'" . beancount-mode)
  :hook ((beancount-mode . (lambda ()
                             (setq-local electric-indent-chars nil)))
         (beancount-mode . outline-minor-mode))
  :custom
  (beancount-highlight-transaction-at-point t))

(use-package org-gtd
  :load-path "~/.emacs.d/packages/org-gtd.el/" "~/.emacs.d/packages/org-edna/"
  :hook (after-init . org-gtd-mode)
  :bind ("<f12>" . my/agenda-menu)
  :init
  (setq org-gtd-update-ack "4.0.0")
  (setopt org-gtd-directory (expand-file-name "iCloud~com~appsonthemove~beorg/Documents/org" icloud))
  :custom
  (org-gtd-refile-to-any-target nil)
  (org-use-fast-todo-selection 'expert)
  (org-gtd-clarify-show-horizons 'right)
  (org-gtd-clarify-display-helper-buffer t)
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|" "DONE(d)" "CNCL(c)")))
  (org-gtd-keyword-mapping '((todo . "TODO")
                             (next . "NEXT")
                             (wait . "WAIT")
			     (done . "DONE")
                             (canceled . "CNCL")))
  (org-gtd-areas-of-focus '("Work" "Professional" "Health" "Growth" "Finances" "Leisure" "Home" "Family" "Social"))
  :config
  (org-edna-mode 1))

(unless (featurep 'org-gtd-wip)
  (require 'org-gtd-wip))

(with-eval-after-load 'org
  (setopt org-agenda-files (list org-gtd-directory)))

(with-eval-after-load 'org-agenda
  (unless (featurep 'org-gtd-agenda-transient)
    (require 'org-gtd-agenda-transient)))

(with-eval-after-load 'org-gtd-clarify
  (unless (featurep 'org-gtd-organize)
    (require 'org-gtd-organize))
  (define-key org-gtd-clarify-mode-map (kbd "C-c C-c") #'org-gtd-organize))

;; Sync org entry with clocking to MacOS Calendar.

(use-package async
  :load-path "~/.emacs.d/packages/emacs-async/")

(defgroup org2calendar nil
  "Sync Org clocking to macOS Calendar."
  :group 'org-clock)

(defcustom org2calendar-target-calendar "Clocking"
  "The name of the macOS Calendar to sync to."
  :type 'string
  :group 'org2calendar)

(defun org2calendar--format-for-applescript (time-val)
  "将时间转换为 (年 月 日 时 分 秒) 的数字列表。"
  (let ((time (if (stringp time-val) (org-time-string-to-time time-val) time-val)))
    (mapcar #'string-to-number
            (split-string (format-time-string "%Y %m %d %H %M %S" time)))))

(defun my/create-applescript (start-list end-list summary)
  "通过直接设置属性的方式构建日期。"
  (let ((s-y (nth 0 start-list)) (s-m (nth 1 start-list)) (s-d (nth 2 start-list))
        (s-h (nth 3 start-list)) (s-min (nth 4 start-list)) (s-s (nth 5 start-list))
        (e-y (nth 0 end-list)) (e-m (nth 1 end-list)) (e-d (nth 2 end-list))
        (e-h (nth 3 end-list)) (e-min (nth 4 end-list)) (e-s (nth 5 end-list)))
    (format
     "tell application \"Calendar\"
        tell calendar \"%s\"
          set theStart to (current date)
          set year of theStart to %s
          set month of theStart to %s
          set day of theStart to %s
          set hours of theStart to %s
          set minutes of theStart to %s
          set seconds of theStart to %s

          set theEnd to (current date)
          set year of theEnd to %s
          set month of theEnd to %s
          set day of theEnd to %s
          set hours of theEnd to %s
          set minutes of theEnd to %s
          set seconds of theEnd to %s

          make new event at end with properties {summary:\"%s\", start date:theStart, end date:theEnd}
        end tell
      end tell"
     org2calendar-target-calendar
     s-y s-m s-d s-h s-min s-s
     e-y e-m e-d e-h e-min e-s
     (replace-regexp-in-string "\"" "\\\"" summary))))

(defun org2calendar-get-context-summary ()
  "获取标题摘要。
如果父标题存在且不属于特定的过滤词（Actions, Calendar, Projects），
则返回 '父标题 / 当前标题'。"
  (let* ((heading (org-get-heading t t t t))
         (excluded-parents '("Actions" "Calendar" "Projects"))
         (parent-title (save-excursion
                         (when (org-up-heading-safe)
                           (org-get-heading t t t t)))))
    (if (and parent-title
             (not (member parent-title excluded-parents)))
        (format "%s / %s" parent-title heading)
      heading)))

(defun org2calendar-sync (start end summary)
  "Generate AppleScript and execute it asynchronously with error handling."
  (condition-case err
      (let ((apple-script (my/create-applescript start end summary)))
        (message "Syncing to Calendar: %s..." summary)
        (if (and (featurep 'async) (display-graphic-p))
            (async-start
             `(lambda ()
                (shell-command-to-string (format "osascript -e %s" (shell-quote-argument ,apple-script))))
             (lambda (result)
               (message "Calendar sync completed.")))
          ;; Synchronous fallback
          (shell-command-to-string (format "osascript -e %s" (shell-quote-argument apple-script)))
          (message "Calendar sync completed.")))
    (error (message "Org2Calendar Sync failed: %s" (error-message-string err)))))

(defun org2calendar-handle-clock-out ()
  "Handler for `org-clock-out-hook`."
  (save-excursion
    ;; org-clock-start-time and org-clock-out-time are available here
    (let* ((summary (org2calendar-get-context-summary))
           (start (org2calendar--format-for-applescript org-clock-start-time))
           (end (org2calendar--format-for-applescript org-clock-out-time)))
      (if (and start end)
          (org2calendar-sync start end summary)
        (message "Org2Calendar: Could not determine clock duration.")))))

(add-hook 'org-clock-out-hook #'org2calendar-handle-clock-out)

(defun org2calendar-sync-at-point ()
  "Parse the CLOCK line at point and sync it to MacOS Calendar."
  (interactive)
  (let* ((element (org-element-at-point))
         (type (org-element-type element)))
    (if (eq type 'clock)
        (let* ((value (org-element-property :value element))
               (start-ts (org-format-timestamp value "%Y-%m-%d %H:%M:%S"))
               (end-ts (org-format-timestamp value "%Y-%m-%d %H:%M:%S" t))
               (summary (org2calendar-get-context-summary))
               (start (org2calendar--format-for-applescript start-ts))
               (end (org2calendar--format-for-applescript end-ts)))

          ;; Debug message to verify parsing
          (message "Syncing: %s from %s to %s" summary start-ts end-ts)

          (if (and start end)
              (org2calendar-sync start end summary)
            (user-error "Could not format timestamps for AppleScript")))
      (user-error "Cursor is not on a CLOCK line (detected: %s)" type))))

(transient-define-prefix my/agenda-menu ()
  "GTD"
  [["Agenda"
    ("a" "Agenda" org-agenda :transient nil)
    ("<f12>" "Engage" org-gtd-engage)
    ;; ("b" "Book" my/book-agenda :transient nil)
    ;; ("t" "TODO" my/all-todo-agenda :transient nil)
    ]
   ["Capture & Process"
    ("C-<f12>" "Capture" org-gtd-capture)
    ("x" "Process Inbox" org-gtd-process-inbox :transient nil)
    ("@" "By Context" org-gtd-engage-grouped-by-context :transient nil)
    ;; ("<f12>" "Engage" my/org-gtd-engage :transient nil)
    ]
   ["Clarify"
    ("c" "Item" org-gtd-clarify-item :transient nil)
    ("C" "Item: agenda" org-gtd-clarify-agenda-item :transient nil)]
   ["Review"
    ("A" "Archive" org-gtd-archive-completed-items)
    ("o" "Missed Appointments" org-gtd-oops :transient t)
    ;; ("m" "Missed Items" org-gtd-review-missed-items :transient t)
    ("f" "Area of Focus" org-gtd-review-area-of-focus :transient t)
    ;; ("s" "Stucks" my/gtd-stuck-menu :transient t)
    ]
   ["Misc"
    ("s" "Sync Calendar" org2calendar-sync-at-point)]])

(transient-define-prefix my/org-gtd-agenda-transient ()
  [[:if org-gtd-agenda-transient--show-time-p
   "Time"
   ("+" "Defer 1 day" org-gtd-agenda-transient--defer)
   ("s" "Set date" org-gtd-agenda-transient--set-date)]
  ["Metadata"
   ("a" "Area of focus" org-gtd-agenda-transient--area-of-focus)]
  ["Clarify"
   ("c" "Clarify (refile)" org-gtd-agenda-transient--clarify-refile)
   ("C" "Clarify (in place)" org-gtd-agenda-transient--clarify-in-place)]])

(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "C-.") #'my/org-gtd-agenda-transient))

(defun my/wallpaper-set ()
  (interactive)
  (wallpaper-set (buffer-file-name)))

(use-package snow
  :load-path "~/.emacs.d/packages/snow.el/"
  :commands snow)

(defvar-keymap my/file-prefix-map
  :doc "Prefix map for file."
  "f" #'find-file
  "b" #'ibooks-annot/open-book-with-ibooks
  "w" #'find-file-other-window
  "j" #'find-file-other-window-no-jump
  "p" #'find-file-at-point
  "t" #'find-file-other-tab
  "r" #'consult-recent-file
  ;; "e" #'my/open-link-with-eww
  "F" #'macos-reveal-in-finder
  "s" #'macos-share
  ;; "a" #'my/consult-find-attach
  ;; "c" #'my/copy-file-info
  ;; "i" #'my/denote-info
  "u" #'update-org-attach-property)

(keymap-set global-map "C-c f" my/file-prefix-map)

(load (setq custom-file (locate-user-emacs-file "custom.el")) t)

(add-hook 'window-setup-hook
          (lambda ()
            (garbage-collect)
            (let ((curtime (current-time)))
              (message "Times: init:%.06f total:%.06f gc-done:%d"
                       (float-time (time-subtract after-init-time before-init-time))
                       (float-time (time-subtract curtime before-init-time))
                       gcs-done)))
          90)
