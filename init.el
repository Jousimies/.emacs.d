;; init.el --- Personal Emacs Configuration -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(setq auto-mode-case-fold nil)

(unless (or (daemonp) noninteractive init-file-debug)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist
                                           old-file-name-handler-alist)))))))

(add-hook 'window-setup-hook
          (lambda ()
            (garbage-collect)
            (let ((curtime (current-time)))
              (message "Times: init:%.06f total:%.06f gc-done:%d"
                       (float-time (time-subtract after-init-time before-init-time))
                       (float-time (time-subtract curtime before-init-time))
                       gcs-done)))
          90)

(defun sanityinc/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))

(defvar sanityinc/require-times nil
  "A list of (FEATURE LOAD-START-TIME LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defun sanityinc/require-times-wrapper (orig feature &rest args)
  "Note in `sanityinc/require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        (apply orig feature args)
      (when (and (not already-loaded) (memq feature features))
        (let ((time (sanityinc/time-subtract-millis (current-time) require-start-time)))
          (add-to-list 'sanityinc/require-times
                       (list feature require-start-time time)
                       t))))))

(advice-add 'require :around 'sanityinc/require-times-wrapper)

(define-derived-mode sanityinc/require-times-mode tabulated-list-mode "Require-Times"
  "Show times taken to `require' packages."
  (setq tabulated-list-format
        [("Start time (ms)" 20 sanityinc/require-times-sort-by-start-time-pred)
         ("Feature" 30 t)
         ("Time (ms)" 12 sanityinc/require-times-sort-by-load-time-pred)])
  (setq tabulated-list-sort-key (cons "Start time (ms)" nil))
  ;; (setq tabulated-list-padding 2)
  (setq tabulated-list-entries #'sanityinc/require-times-tabulated-list-entries)
  (tabulated-list-init-header)
  (when (fboundp 'tablist-minor-mode)
    (tablist-minor-mode)))

(defun sanityinc/require-times-sort-by-start-time-pred (entry1 entry2)
  (< (string-to-number (elt (nth 1 entry1) 0))
     (string-to-number (elt (nth 1 entry2) 0))))

(defun sanityinc/require-times-sort-by-load-time-pred (entry1 entry2)
  (> (string-to-number (elt (nth 1 entry1) 2))
     (string-to-number (elt (nth 1 entry2) 2))))

(defun sanityinc/require-times-tabulated-list-entries ()
  (cl-loop for (feature start-time millis) in sanityinc/require-times
           with order = 0
           do (cl-incf order)
           collect (list order
                         (vector
                          (format "%.3f" (sanityinc/time-subtract-millis start-time before-init-time))
                          (symbol-name feature)
                          (format "%.3f" millis)))))

(defun sanityinc/require-times ()
  "Show a tabular view of how long various libraries took to load."
  (interactive)
  (with-current-buffer (get-buffer-create "*Require Times*")
    (sanityinc/require-times-mode)
    (tabulated-list-revert)
    (display-buffer (current-buffer))))

(defun sanityinc/show-init-time ()
  (message "init completed in %.2fms"
           (sanityinc/time-subtract-millis after-init-time before-init-time)))

(defun set-exec-path-from-shell-PATH ()
  "This is particularly useful under Mac OS X and macOS."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$" "" (shell-command-to-string
                                          "$SHELL --login -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-PATH)

(require 'cl-lib)
(defun add-subdirs-to-load-path (search-dir)
  (interactive)
  (let* ((dir (file-name-as-directory search-dir)))
    (dolist (subdir
	     ;; 过滤出不必要的目录，提升Emacs启动速度
	     (cl-remove-if
	      #'(lambda (subdir)
		  (or
		   ;; 不是目录的文件都移除
		   (not (file-directory-p (concat dir subdir)))
		   ;; 父目录、 语言相关和版本控制目录都移除
		   (member subdir '("." ".."
				    "dist" "node_modules" "__pycache__"
				    "RCS" "CVS" "rcs" "cvs" ".git" ".github"))))
	      (directory-files dir)))
      (let ((subdir-path (concat dir (file-name-as-directory subdir))))
	;; 目录下有 .el .so .dll 文件的路径才添加到 `load-path' 中，提升Emacs启动速度
	(when (cl-some #'(lambda (subdir-file)
			   (and (file-regular-p (concat subdir-path subdir-file))
				;; .so .dll 文件指非Elisp语言编写的Emacs动态库
				(member (file-name-extension subdir-file) '("el" "so" "dll"))))
		       (directory-files subdir-path))

	  ;; 注意：`add-to-list' 函数的第三个参数必须为 t ，表示加到列表末尾
	  ;; 这样Emacs会从父目录到子目录的顺序搜索Elisp插件，顺序反过来会导致Emacs无法正常启动
	  (add-to-list 'load-path subdir-path t))

	;; 继续递归搜索子目录
	(add-subdirs-to-load-path subdir-path)))))
(add-subdirs-to-load-path "~/.emacs.d/packages")
;; (add-to-list 'load-path "~/.emacs.d/packages")

(use-package on)

(defvar my-cloud "~/Nextcloud"
  "This folder is My cloud.")

;; L.Personal.Galaxy location may change, but folders in this directory never change.
(defvar my-galaxy (expand-file-name "L.Personal.Galaxy" my-cloud)
  "This folder stores all the plain text files of my life.")

(defvar website-directory "~/Nextcloud/L.Personal.Galaxy/website"
  "The source folder of my blog.")

(use-package evil
  :bind (:map evil-insert-state-map
              ("C-e" . move-end-of-line)
              ("C-k" . kill-line))
  :hook ((after-init . evil-mode)
         (after-change-major-mode . (lambda ()
                                      (setq-local evil-shift-width tab-width))))
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-h-delete t)
  (setq evil-respect-visual-line-mode t)
  :config
  (setq evil-undo-system 'undo-fu)
  (setq evil-visual-state-cursor 'hollow)

  (setq evil-normal-state-tag " 𝐍 ")
  (setq evil-insert-state-tag " 𝐈 ")
  (setq evil-motion-state-tag " 𝐌 ")
  (setq evil-visual-state-tag " 𝐕 ")
  (setq evil-replace-state-tag " 𝐑 ")
  (setq evil-operator-state-tag " O ")
  (setq evil-emacs-state-tag " E "))

(global-set-key (kbd "C-M-u") 'universal-argument)

;; (with-eval-after-load 'evil
;;   (evil-collection-init))
(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-key-blacklist '("SPC" ","))
  (setq forge-add-default-bindings nil)
  (evil-collection-init))

(use-package evil-commentary
  :hook (on-first-file . evil-commentary-mode))

(use-package evil-surround
  :hook (on-first-file . global-evil-surround-mode))

(use-package evil-embrace
  :hook (org-mode . embrace-org-mode-hook)
  :config
  (evil-embrace-enable-evil-surround-integration))

(use-package which-key
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 10000)
  (setq which-key-idle-secondary-delay 0.05))

(use-package general
  :config
  (general-create-definer my/space-leader-def
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
    :states '(normal visual insert emacs)))
;; (general-create-definer my/comma-leader-def
;;               :prefix ","
;;               :non-normal-prefix "M-,"
;;               :states '(normal visual insert emacs))

(defun my/emacs-config ()
  "My literate Emacs configuration."
  (interactive)
  (find-file (expand-file-name "emacs.org" user-emacs-directory)))

(my/space-leader-def
  "f" '(:ignore t :wk "Files"))

(with-eval-after-load 'evil
  (evil-define-key '(normal motion visual) 'global
    "ge" nil
    "gn" nil))

(use-package epkg
  :commands (epkg-describe-package)
  :config
  (setq epkg-repository (expand-file-name "cache/epkgs" user-emacs-directory)))

(use-package epkg-marginalia
  :after (epkg marginalia)
  :config
  (cl-pushnew 'epkg-marginalia-annotate-package
              (alist-get 'package marginalia-annotator-registry)))

(use-package auto-save
  :hook (on-first-file . auto-save-enable)
  :config
  (setq auto-save-silent t)
  (setq auto-save-delete-trailing-whitespace t))

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

(use-package server
  :hook (on-first-input . server-start)
  :config
  (defun my/start-server ()
    (interactive)
    (if (not (server-running-p))
        (server-start))
    (message "Server has started")))

(my/space-leader-def
  "q" '(:ignore t :wk "Quit/Restart")
  "qR" '(restart-emacs :wk "Restart emacs")
  "qq" '(server-force-delete :wk "Server Delete")
  "qs" '(my/start-server :wk "Server Delete"))

(use-package magit
  :commands (magit magit-status magit-submodule-add)
  :bind ("C-x g" . magit)
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-module-sections-hook 'magit-insert-modules-overview)
  (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpulled-from-pushremote)
  (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpushed-to-upstream)
  (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpushed-to-pushremote))

(use-package transient
  :config
  (setq transient-levels-file (expand-file-name "cache/transient/levels.el" user-emacs-directory))
  (setq transient-values-file (expand-file-name "cache/transient/values.el" user-emacs-directory))
  (setq transient-history-file (expand-file-name "cache/transient/history.el" user-emacs-directory)))

(use-package forge
  :after magit
  :config
  (setq forge-database-file (expand-file-name "cache/forge-database.sqlite" user-emacs-directory)))

(use-package browse-at-remote
  :commands browse-at-remote)

(my/space-leader-def
    "or" '(browse-at-remote :wk "Open remote"))

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(set-face-attribute 'default nil :font "Iosevka Term" :height 160)
(if (display-graphic-p)
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset (font-spec :family "Source Han Serif SC" :height 140)) t 'prepend))

(use-package modus-themes
  :config
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-italic-constructs t)

  (setq modus-themes-common-palette-overrides
    '(;; mode-line
      (border-mode-line-active unspecified)
      (border-mode-line-inactive unspecified)
      (bg-mode-line-active bg-main)
      (fg-mode-line-active fg-main)

      ;; line-number
      (fg-line-number-inactive "gray50")
      (fg-line-number-active red-cooler)
      (bg-line-number-inactive unspecified)
      (bg-line-number-active unspecified)
      ;; link
      (underline-link border)
      (underline-link-visited border)
      (underline-link-symbolic border)

      ;; org agenda
      (date-common cyan)   ; default value (for timestamps and more)
      (date-deadline red-warmer)
      (date-event magenta-warmer)
      (date-holiday blue) ; for M-x calendar
      (date-now yellow-warmer)
      (date-scheduled magenta-cooler)
      (date-weekday cyan-cooler)
      (date-weekend blue-faint)

      ;; org heading
      (fg-heading-1 blue-warmer)
      (fg-heading-2 yellow-cooler)
      (fg-heading-3 cyan-cooler)))

  (setq modus-themes-prompts '(extrabold italic))

  (setq modus-themes-completions
    '((matches . (extrabold))
      (selection . (semibold italic text-also)))))

(load-theme 'modus-operandi t)

(define-fringe-bitmap 'right-curly-arrow  [])
(define-fringe-bitmap 'left-curly-arrow  [])

(fringe-mode '(1 . 1))

(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'org-mode-hook 'hl-line-mode)

(setq-default display-line-numbers-widen t)
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'org-mode-hook 'display-line-numbers-mode)
(add-hook 'LaTeX-mode-hook 'display-line-numbers-mode)

(scroll-bar-mode 0)

(setq-default fill-column 90)

(face-spec-set 'fill-column-indicator
               '((default :height 0.1))
               'face-override-spec)

;; only show fill indicator in prog mode.
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

(setq show-paren-style 'parenthesis)
(setq show-paren-context-when-offscreen 'overlay)

(add-hook 'text-mode-hook 'show-paren-mode)

(blink-cursor-mode -1)

(defun im--chinese-p ()
  "Check if the current input state is Chinese."
  (if (featurep 'rime)
      (and (rime--should-enable-p)
           (not (rime--should-inline-ascii-p))
           current-input-method)
    current-input-method))

(add-hook 'evil-insert-state-entry-hook
          (lambda ()
            (when (im--chinese-p)
              (set-cursor-color "red"))))

(add-hook 'evil-insert-state-exit-hook
          (lambda ()
            (set-cursor-color (foreground-color-at-point))))

(set-frame-parameter nil 'alpha '(90 . 100))

(defun ct/frame-center (&optional frame)
  "Center a frame on the screen."
  (interactive)
  (let* ((frame (or (and (boundp 'frame) frame) (selected-frame)))
         (center (ct/frame-get-center frame)))
    (apply 'set-frame-position (flatten-list (list frame center)))))

(defun ct/screen-usable-height (&optional display)
  "Return the usable height of the display.

Some window-systems have portions of the screen which Emacs
cannot address. This function should return the height of the
screen, minus anything which is not usable."
  (- (display-pixel-height display)
     (cond ((eq window-system 'ns) 22) ;; macOS Menu Bar offset
           (t 0))))

(defun ct/screen-usable-width (&optional display)
  "Return the usable width of the display."
  (display-pixel-width display))

(defun ct/center-box (w h cw ch)
  "Center a box inside another box.

Returns a list of `(TOP LEFT)' representing the centered position
of the box `(w h)' inside the box `(cw ch)'."
  (list (/ (- cw w) 2) (/ (- ch h) 2)))

(defun ct/frame-get-center (frame)
  "Return the center position of FRAME on it's display."
  (let ((disp (frame-parameter frame 'display)))
    (ct/center-box (frame-pixel-width frame) (frame-pixel-height frame)
                   (ct/screen-usable-width disp) (ct/screen-usable-height disp))))

(defun ct/frame-center (&optional frame)
  "Center a frame on the screen."
  (interactive)
  (apply 'set-frame-position
         (let* ((frame (or (and (boundp 'frame) frame) (selected-frame)))
                (center (ct/frame-get-center frame)))
           ;; Flatten the X/Y list in `center` into a single list with `frame`
           ;; so this list can be applied as parameters to `set-frame-position`:
           `(,frame ,@center))))

(add-to-list 'after-make-frame-functions #'ct/frame-center 0)

(defun my/make-or-delete-frame ()
  (interactive)
  (if (= (frame-width) 80) ;; 80 is the default frame width.
      (delete-frame)
    (make-frame)))

(global-set-key (kbd "s-n") 'my/make-or-delete-frame)

(defun my/mode-line-padding ()
  (let* ((r-length (string-width (format-mode-line global-mode-string))))
    (propertize " "
                'display `(space :align-to (- right ,(+ r-length 1))))))

(add-to-list 'global-mode-string
             '(:eval (propertize
                      (concat
                       "𝚻𝚨𝚩 "
                       (number-to-string (tab-bar--current-tab-index))
                       ": "
                       (alist-get 'group (tab-bar--current-tab))) 'face 'font-lock-constant-face)))

(setq mode-line-end-spaces
      '(""
        global-mode-string))

(setq mode-line-position-column-line-format '(" %l,%c"))

(setq mode-line-percent-position '(-4 "%p"))

(setq-default mode-line-format
              `("%e"
                mode-line-front-space
                (:propertize ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote))
                mode-line-frame-identification
                mode-line-buffer-identification
                mode-line-position
                ;; (:eval (propertize " %I " 'face 'font-lock-constant-face))
                "  "
                (vc-mode vc-mode)
                (:eval (when buffer-read-only
                         (concat "  "  (propertize "RO"
                                                   'face 'font-lock-type-face
                                                   'help-echo "Buffer is read-only"))))
                (:eval (my/mode-line-padding))
                mode-line-end-spaces))

(use-package mode-line-bell
  :hook (on-first-buffer . mode-line-bell-mode))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(evil-paste-after
                   evil-paste-pop
                   evil-paste-before
                   evil-delete
                   evil-delete-line))
  (advice-add command :after #'pulse-line))

(use-package color-identifiers-mode
  :hook (on-first-file . global-color-identifiers-mode))

(use-package page-break-lines
  :hook (org-mode . global-page-break-lines-mode))

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

(use-package undo-fu)

(use-package undo-fu-session
  :after undo-fu
  :hook (on-first-file . undo-fu-session-global-mode)
  :config
  (setq undo-fu-session-directory (expand-file-name "cache/undo-fu-session" user-emacs-directory)))

(use-package vundo
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  :bind ("C-x u" . vundo))

(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.Trash")

(use-package hungry-delete
  :custom
  (hungry-delete-chars-to-skip " \t\n\r\f\v")
  :hook ((text-mode . hungry-delete-mode)
         (prog-mode . hungry-delete-mode)
         (org-mode . hungry-delete-mode)))

(use-package gc-buffers
  :hook (on-first-buffer . gc-buffers-mode))

(use-package ace-window
  :bind ("C-x o" . ace-window))

(use-package rime
  :init
  (setq rime-title "𝐑 ")
  :config
  (setq default-input-method "rime")
  (setq rime-user-data-dir "~/Library/Rime/")
  (setq rime-emacs-module-header-root "/Applications/Emacs.app/Contents/Resources/include/")
  (setq rime-librime-root (expand-file-name "librime/dist" user-emacs-directory))
  (setq rime-show-candidate 'minibuffer)
  ;; (setq rime-posframe-properties '(:internal-border-width 0))
  (setq rime-disable-predicates '(rime-predicate-prog-in-code-p
                                  rime-predicate-org-in-src-block-p
                                  rime-predicate-org-latex-mode-p
                                  rime-predicate-tex-math-or-command-p))

  (setq rime-inline-predicates '(rime-predicate-space-after-cc-p
                                 rime-predicate-after-alphabet-char-p))
  :bind (:map rime-mode-map
              ("M-j" . rime-force-enable))
  :hook ((evil-insert-state-entry . (lambda ()
                                      (if (and (not (rime--should-inline-ascii-p))
                                               (eq major-mode 'org-mode)
                                               (not (org-at-clock-log-p))
                                               (not (org-at-table-p))
                                               (not (org-at-timestamp-p))
                                               (not (and (bolp) (org-on-heading-p))))
                                          (activate-input-method "rime"))))
         (evil-insert-state-exit .  #'evil-deactivate-input-method)))

(use-package rime-regexp
  :hook (on-first-input . rime-regexp-mode))

(use-package helpful
  :commands helpful-update
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key))
  :init
  (setq help-window-select 'always)
  (setq help-window-keep-selected t)
  :config
  (add-to-list 'display-buffer-alist
               '((or (derived-mode . help-mode)
                     (derived-mode . helpful-mode))
                 (display-buffer-reuse-mode-window display-buffer-in-side-window)
                 (window-width . 0.5)
                 (side . right)
                 (slot . 0))))

(autoload #'elisp-demos-advice-helpful-update "elisp-demos" nil t)
(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

(use-package expand-region
  :commands er/expand-region)

(with-eval-after-load 'evil
    (evil-define-key 'visual 'evil-visual-state-map
      "v" 'er/expand-region))

(use-package ctrlf
  :after evil
  :hook (on-first-buffer . ctrlf-mode)
  :config
  (evil-global-set-key 'normal (kbd "/") 'ctrlf-forward-default))

(use-package whitespace-cleanup-mode
  :hook (on-first-file . whitespace-cleanup-mode))

(use-package tempel
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert))
  :config
  (setq tempel-path `("~/.emacs.d/template/tempel"
                      ,(expand-file-name "template/tempel" my-galaxy))))

(use-package yasnippet
  :hook (on-first-file . yas-global-mode)
  :config
  (use-package yasnippet-snippets
    :after yasnippet))

(use-package rg
  :hook (on-first-input . rg-enable-default-bindings)
  :config
  (setq rg-group-result t)
  (setq rg-show-columns t))

(setq read-buffer-completion-ignore-case t)
(setq completion-ignore-case t)
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

(use-package simple
  :config
  (setq-default read-extended-command-predicate #'command-completion-default-include-p))

(use-package minibuffer
  :config
  (setq completion-category-overrides '((file (styles basic partial-completion))))
  (setq read-file-name-completion-ignore-case t)

  (setq-local completion-in-region-function
              (lambda (&rest args)
                (apply (if vertico-mode
                           #'consult-completion-in-region
                         #'completion--in-region)
                       args))))

(setq tab-always-indent 'complete)

(use-package orderless
  :config
  (setq completion-styles '(orderless partial-completion)))

(use-package vertico
  :load-path "~/.emacs.d/packages/vertico"
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-cycle t)
  :bind (:map vertico-map
      ("C-j" . vertico-next)
      ("C-k" . vertico-previous)))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
        ("C-u" . vertico-directory-up)))

(use-package marginalia
  :hook ((minibuffer-setup . marginalia-mode)))

(use-package embark
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         (:map vertico-map
               ("C-c C-o" . embark-export)
               ("C-c C-c" . embark-act)))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(my/space-leader-def
  "oe" '(embark-open-externally :wk "Open externally"))

(use-package consult
  :commands consult-outline
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (([remap apropos] . consult-apropos)
         ([remap bookmark-jump] . consult-bookmark)
         ([remap goto-line] . consult-line)
         ([remap imenu] . consult-imenu)
         ([remap locate] . consult-locate)
         ([remap load-theme] . consult-theme)
         ([remap man] . consult-man)
         ([remap recentf-open-files] . consult-recent-file)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap yank-pop] . consult-yank-pop)
         :map minibuffer-mode-map
         ("C-r" . consult-history)))

(with-eval-after-load 'evil
  (evil-declare-key 'normal org-mode-map
    "gh" 'consult-outline)
  (evil-declare-key 'normal LaTeX-mode-map
    "gh" 'consult-outline))

(use-package corfu
  :config
  (setq corfu-cycle t)
  (setq corfu-auto t)
  (setq corfu-auto-prefix 2)
  (setq corfu-auto-delay 0.0)
  (setq corfu-preselect 'valid)

  (setq-default corfu-quit-no-match 'separator)

  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input)
                (eq (current-local-map) read-passwd-map))
      (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  :hook (after-init . global-corfu-mode))

(use-package corfu-echo
  :hook (corfu-mode . corfu-echo-mode))

(use-package corfu-popupinfo
  :hook (corfu-mode . corfu-popupinfo-mode))

(use-package kind-icon
  :commands kind-icon-margin-formatter
  :config
  (setq kind-icon-use-icons t)
  (setq kind-icon-default-face 'corfu-default))

(use-package cape
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(autoload 'prescient-persist-mode "prescient" "" t)
(add-hook 'after-init-hook 'prescient-persist-mode)

(use-package vertico-prescient
  :hook (vertico-mode . vertico-prescient-mode))

(use-package corfu-prescient
  :hook (corfu-mode . corfu-prescient-mode)
  :config
  (setq vertico-prescient-completion-styles '(orderless prescient partial-completion)))

(use-package gcmh
  :config
  (setq gcmh-idle-delay 'auto)
  (setq gcmh-auto-idle-delay-factor 10)
  (setq gcmh-high-cons-threshold #x1000000)
  :hook (after-init . gcmh-mode))

(use-package file-info
  :commands file-info-show
  :config
  (setq hydra-hint-display-type 'posframe)
  (setq hydra-posframe-show-params `(:poshandler posframe-poshandler-frame-center
                                                 :internal-border-width 2
                                                 :internal-border-color "#61AFEF"
                                                 :left-fringe 16
                                                 :right-fringe 16)))

(my/space-leader-def
  "mi" '(file-info-show :wk "File info"))

(use-package disk-usage
  :bind ("C-c d u" . disk-usage))
(my/space-leader-def
  "m" '(:ignore t :wk "Misc")
  "md" '(disk-usage :wk "Disk usage"))

(use-package youtube-dl
  :commands youtube-dl
  :config
  (setq youtube-dl-directory "~/Downloads/")
  (setq youtube-dl-program "/opt/homebrew/bin/youtube-dl")
  (setq youtube-dl-arguments
        '("--no-mtime" "--restrict-filenames" "--format" "best" "--mark-watched")))

(defun my/ocr ()
"OCR with Macos system."
  (interactive)
  (shell-command "shortcuts run \"OCR Selected Area\"")
  (do-applescript "tell application id \"org.gnu.Emacs\" to activate"))

(my/space-leader-def
  "mo" '(my/ocr :wk "OCR"))

(defun toggle-proxy ()
  "Toggle proxy for the url.el library."
  (interactive)
  (if url-proxy-services
      (proxy-disable)
    (proxy-enable)))

(defun proxy-enable ()
  "Enable proxy."
  (interactive)
  (setq url-proxy-services
        '(("http" . "127.0.0.1:8118")
          ("https" . "127.0.0.1:8118")
          ("socks" . "127.0.0.1:8118")
          ("no_proxy" . "0.0.0.0")))
  (message "Proxy enabled! %s" (car url-proxy-services)))

(defun proxy-disable ()
  "Disable proxy."
  (interactive)
  (if url-proxy-services
      (setq url-proxy-services nil))
  (message "Proxy disabled!"))

(run-with-idle-timer 2 nil (lambda ()
                             (proxy-enable)))

(my/space-leader-def
  "mp" '(toggle-proxy :wk "Proxy"))

(use-package advance-words-count
  :bind ("M-=" . advance-words-count))

(defun eps-to-png-marked ()
  "Convert all marked EPS files in the current Dired buffer to PNG format using ImageMagick's convert utility.
Each input file is converted to a PNG file with the same basename.
This function requires ImageMagick's convert utility to be installed and available in the system's PATH."
  (interactive)
  (let ((eps-files (dired-get-marked-files)))
    (when (not eps-files)
      (error "No marked files in Dired buffer."))
    (let ((n 0))
      (message "Converting:\n")
      (dolist (epsfile eps-files)
        (let ((pngfile (concat (file-name-sans-extension epsfile) ".png")))
          (setq n (1+ n))
          (message "%d: %s to %s." n epsfile pngfile)
          (start-process "eps-to-png"
                         "*eps-to-png*"
                         "convert"
                         "-colorspace"
                         "sRGB"
                         "-density"
                         "600x600"
                         epsfile
                         pngfile)))
      (message "\n%d files were converted from EPS to PNG format." n))))

(use-package ispell
  :config
  (setq ispell-program-name "/opt/homebrew/bin/aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))
  (setq ispell-aspell-dict-dir
        (ispell-get-aspell-config-value "dict-dir"))

  (setq ispell-aspell-data-dir
        (ispell-get-aspell-config-value "data-dir"))

  (setq ispell-personal-dictionary (expand-file-name "config/ispell/.aspell.en.pws" my-galaxy))

  (setq-default ispell-following-word t
                ispell-quietly t))

;; Suppress start looking process.
;; https://github.com/company-mode/company-mode/issues/912
;; shut-up
(with-eval-after-load 'ispell
  (advice-add 'ispell-lookup-words :around
              (lambda (orig &rest args)
                (shut-up (apply orig args)))))

(use-package flyspell
  :hook (org-mode . flyspell-mode))

(use-package flyspell-correct
  :commands flyspell-correct-wrapper
  :bind ([remap flyspell-auto-correct-previous-word] . flyspell-correct-wrapper))

(use-package langtool
  :commands langtool-check-buffer
  :config
  (setq langtool-http-server-host "localhost")
  (setq langtool-http-server-port 8081)
  (setq langtool-autoshow-message-function #'langtool-popup-autoshow))

(add-to-list 'display-buffer-alist
             '("^\\*Dictionary\\*"
               (display-buffer-in-side-window)
               (side . right)
               (window-width . 70)))

(global-set-key (kbd "M-#") 'dictionary-lookup-definition)

(use-package go-translate
  :commands gts-do-translate
  :config
  (setq gts-buffer-follow-p t)
  (setq gts-translate-list '(("en" "zh")))
  (setq gts-default-translator (gts-translator
				:picker (gts-noprompt-picker)
				:engines (list
					  (gts-google-engine :parser (gts-google-summary-parser)))
				:render (gts-buffer-render))))
(with-eval-after-load 'evil
  (evil-define-key '(normal visual) 'global
    "gll" 'gts-do-translate))

(use-package lingva
  :commands lingva-translate
  :config
  (setq lingva-target "zh"))

(with-eval-after-load 'evil
  (evil-define-key '(normal visual) 'global
    "glL" 'lingva-translate))

(use-package sdcv
  :commands sdcv-search-pointer sdcv-search-input+
  :config
  (face-spec-set 'sdcv-tooltip-face
                 '((((background light))
                    :foreground "#000000" :background "#ffffff")
                   (t
                    :foreground "#ffffff" :background "#000000"))
                 'face-override-spec)

  (setq sdcv-tooltip-border-width 2)
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

(with-eval-after-load 'evil-collection
    (evil-define-key '(normal visual) 'global
      "glp" 'sdcv-search-pointer
      "gli" 'sdcv-search-input+)

  (evil-collection-define-key 'normal 'sdcv-mode-map
    "q" 'quit-window))

(use-package osx-dictionary
  :commands osx-dictionary-search-pointer)

(with-eval-after-load 'evil
  (evil-define-key '(normal visual) 'global
    "gld" 'osx-dictionary-search-pointer))

(use-package powerthesaurus
  :commands (powerthesaurus-lookup-dwim
             powerthesaurus-lookup-related-dwim
             powerthesaurus-lookup-synonyms-dwim
             powerthesaurus-lookup-antonyms-dwim
             powerthesaurus-lookup-definitions-dwim
             powerthesaurus-lookup-sentences-dwim))

(with-eval-after-load 'evil
  (evil-define-key '(normal visual) 'global
    "glt" 'powerthesaurus-lookup-dwim))

(use-package dictionary-overlay
  :defer 5
  :load-path "~/.emacs.d/packages/dictionary-overlay/"
  :config
  (setq dictionary-overlay-translators '("local" "darwin" "sdcv" "web"))
  (setq dictionary-overlay-user-data-directory
        (expand-file-name "var/dictionary-overlay" user-emacs-directory))
  (setq dictionary-overlay-python "/opt/homebrew/bin/python3.10")
  (dictionary-overlay-start))

(with-eval-after-load 'evil
  (evil-define-key '(normal visual) 'global
    "glr" 'dictionary-overlay-render-buffer
    "glk" 'dictionary-overlay-mark-word-unknown
    "glK" 'dictionary-overlay-mark-word-known))

(use-package flycheck
  :hook (prog-mode . flycheck-mode))

(use-package flycheck-grammarly
  :after flycheck
  :commands flycheck-grammarly-setup)

(add-to-list 'display-buffer-alist
             (cons
              "\\*Async Shell Command\\*.*"
              (cons #'display-buffer-no-window nil)))

(defun my/siri-translate ()
  (interactive)
  (let ((tempfile (make-temp-file "siri-translate-" nil ".txt")))
    (write-region
     (format "%s" (thing-at-point 'paragraph)) nil tempfile)
    (end-of-paragraph-text)
    (shell-command (format "shortcuts run \"Translate File\" -i %s &" tempfile)))
  (shell-command "open -b org.gnu.Emacs"))

(defun my/siri-translate2english ()
  (interactive)
  (let ((tempfile (make-temp-file "siri-translate-" nil ".txt")))
    (write-region
     (format "%s" (thing-at-point 'paragraph)) nil tempfile)
    (end-of-paragraph-text)
    (shell-command (format "shortcuts run \"Translate File 2 English\" -i %s &" tempfile)))
  (shell-command "open -b org.gnu.Emacs"))

(defun language-to-zh-or-zh-to-english ()
  (interactive)
  (let ((string (thing-at-point 'paragraph)))
    (if (eq (string-match "\\cC" string) nil)
        (my/siri-translate)
      (my/siri-translate2english))))

(with-eval-after-load 'yasnippet
  (add-hook 'LaTeX-mode-hook 'eglot-ensure))

(use-package markdown-mode
  :mode (("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

;; (with-eval-after-load 'whitespace-cleanup-mode
;;   (add-to-list 'whitespace-cleanup-mode-ignore-modes 'markdown-mode))

(use-package yaml-mode
  :mode ("\\.yaml\\'" . yaml-mode))
  ;; :config
  ;; (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))

(use-package org
  :config
  (setq org-ellipsis " ⇲")
  (setq org-modules '()
        org-imenu-depth 4
        org-return-follows-link t
        org-image-actual-width nil
        org-display-remote-inline-images 'download
        org-log-into-drawer t
        org-fast-tag-selection-single-key 'expert
        org-adapt-indentation nil
        org-fontify-quote-and-verse-blocks t
        org-support-shift-select t
        org-treat-S-cursor-todo-selection-as-state-change nil
        org-hide-leading-stars nil
        org-startup-with-inline-images t
        org-image-actual-width '(500)
        org-use-speed-commands t)
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)

  (setq org-todo-repeat-to-state t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "INPROGRESS(i)" "|" "WAIT(w@)" "SOMEDAY(s@)" "CNCL(c@/!)" "DONE(d)")))

  (setq org-todo-keyword-faces
        '(("NEXT" . '(success org-todo))
          ("TODO" . org-todo)
          ("CNCL" . '(region org-todo))
          ("WAIT" . '(bold org-todo))))

  (setq org-priority-faces
        '((?A . '(bold org-priority))
          (?B . org-priority)
          (?C . '(shadow org-priority))))
  (setq org-todo-state-tags-triggers
        (quote (("CNCL" ("CNCL" . t))
                ("WAIT" ("WAIT" . t))
                ("SOMEDAY" ("WAIT") ("SOMEDAY" . t))
                (done ("WAIT") ("SOMEDAY"))
                ("TODO" ("WAIT") ("CNCL") ("SOMEDAY"))
                ("NEXT" ("WAIT") ("CNCL") ("SOMEDAY"))
                ("DONE" ("WAIT") ("CNCL") ("SOMEDAY")))))
  :bind (:map org-mode-map
              ("C-c l" . org-store-link)))
;; ("<return>" . org-return)))

(use-package ob-core
  :after org
  :config
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
  (advice-add 'org-babel-execute-src-block :before 'my/org-babel-execute-src-block)
  (setq org-confirm-babel-evaluate nil))

(use-package org-capture
  :after org
  :bind (:map org-capture-mode-map
              ([remap evil-save-and-close] . org-capture-finalize)
              ([remap evil-save-modified-and-close] . org-capture-finalize)
              ([remap evil-quit] . org-capture-kill)
              ("RET" . org-capture-finalize))
  :config
  (setq org-capture-templates
        '(("i" "GTD Inbox"
           entry (file (lambda () (concat my-galaxy "/todos/inbox.org")))
           "* %?\n%U\n" :time-prompt t :tree-type week)
          ("I" "NC Inbox"
           plain (file+olp+datetree (lambda () (concat my-galaxy "/inbox/inbox.org")))
           "**** %?\n%U\n" :time-prompt t :tree-type week)
          ("p" "Daily Plan"
           plain (file+olp+datetree (lambda () (concat my-galaxy "/inbox/plan.org")))
           "- [ ] %?\n%U\n" :time-prompt t :tree-type week)
          ("r" "Reflection"
           plain
           (file+olp+datetree (lambda () (concat my-galaxy "/roam/main/reflection.org")))
           (file "~/.emacs.d/template/tpl-daily-reflection")
           :time-prompt t :tree-type week)
          ("a" "Anki Deck")
          ("ae" "Deck: English"
           entry (file (lambda ()
                         (concat my-galaxy "/anki/anki_english.org")))
           "* %?\n" :jump-to-captured t)
          ("ac" "Deck: Civil Engineering"
           entry (file (lambda ()
                         (concat my-galaxy "/anki/anki_engineering.org")))
           "* %?\n" :jump-to-captured t)
          ("s" "Code snippets"
           entry (file (lambda ()
                         (concat my-galaxy "/scripts/snippets.org")))
           "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
          ;; ("l" "Lists")
          ("m" "Movie"
           entry (file+headline (lambda () (concat my-galaxy "/roam/main/watchlist.org")) "Watching Lists")
           "* %?
:PROPERTIES:
:GENRE: %^{Film genre|Action|Adventure|Comedy|Drama|Fantasy|Horror|Musicals|Mystery|Romance|Science fiction|Sports|Thriller}
:COUNTRY:
:SCORE:
:PLOT: %^{PLOT}
:END:"))))

(defun my/org-capture ()
  "Make a new frame to do org-capture staff."
  (interactive)
  (make-frame)
  (org-capture))

(add-hook 'org-capture-after-finalize-hook 'delete-frame)

;; (add-to-list 'display-buffer-alist '("\\*Org Select\\*"
;;                                      (display-buffer-pop-up-frame)
;;                                      (window-parameters
;;                                       (no-other-window . t)
;;                                       (mode-line-format . none)
;;                                       (no-delete-other-windows . t))))

(global-set-key (kbd "<f10>") 'my/org-capture)

(use-package org-attach
  :config
  (setq org-attach-id-dir (expand-file-name "attach" my-galaxy))
  (defun my/org-attach-visit-headline-from-dired ()
    "Go to the headline corresponding to this org-attach directory."
    (interactive)
    (let* ((id-parts (last (split-string default-directory "/" t) 2))
           (id (apply #'concat id-parts)))
      (let ((m (org-id-find id 'marker)))
        (unless m (user-error "Cannot find entry with ID \"%s\"" id))
        (pop-to-buffer (marker-buffer m))
        (goto-char m)
        (move-marker m nil)
        (org-fold-show-context))))
  (add-to-list 'display-buffer-alist
               '("\\*Org Attach\\*"
                 (display-buffer-pop-up-frame)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.5)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))
  :bind (:map dired-mode-map
              ("C-'" . my/org-attach-visit-headline-from-dired)))

(use-package org-habit
  :after org-agenda
  :config
  (add-to-list 'org-modules 'org-habit t))

(use-package org-id
  :after org
  :config
  (setq org-id-locations-file (expand-file-name "cache/.org-id-locations" user-emacs-directory))
  (setq org-id-method 'ts)
  (setq org-id-link-to-org-use-id 'create-if-interactive))

(defun my/copy-idlink ()
  "Copy idlink to clipboard."
  (interactive)
  (when (eq major-mode 'org-agenda-mode) ;switch to orgmode
    (org-agenda-show)
    (org-agenda-goto))
  (when (eq major-mode 'org-mode) ; do this only in org-mode buffers
    (let* ((mytmphead (nth 4 (org-heading-components)))
           (mytmpid (funcall 'org-id-get-create))
           (mytmplink (format "- [ ] [[id:%s][%s]]" mytmpid mytmphead)))
      (kill-new mytmplink)
      (message "Copied %s to killring (clipboard)" mytmplink))))

(global-set-key (kbd "<f7>") 'my/copy-idlink)

(my/space-leader-def
  "oi" '(org-id-get-create :wk "Create ID"))

(use-package org-src
  :after org
  :config
  (setq org-src-window-setup 'current-window)
  (setq org-src-ask-before-returning-to-edit-buffer nil))

(use-package org-refile
  :after org
  :config
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-active-region-within-subtree t))

(use-package org
  :config
  (defface my-org-emphasis-bold
    '((default :inherit bold)
      (((class color) (min-colors 88) (background light))
       :foreground "#a60000")
      (((class color) (min-colors 88) (background dark))
       :foreground "#ff8059"))
    "My bold emphasis for Org.")

  (defface my-org-emphasis-italic
    '((default :inherit italic)
      (((class color) (min-colors 88) (background light))
       :foreground "#005e00")
      (((class color) (min-colors 88) (background dark))
       :foreground "#44bc44"))
    "My italic emphasis for Org.")

  (defface my-org-emphasis-underline
    '((default :inherit underline)
      (((class color) (min-colors 88) (background light))
       :foreground "#813e00")
      (((class color) (min-colors 88) (background dark))
       :foreground "#d0bc00"))
    "My underline emphasis for Org.")

  (defface my-org-emphasis-strike-through
    '((default :strike-through t)
      (((class color) (min-colors 88) (background light))
       :foreground "#505050")
      (((class color) (min-colors 88) (background dark))
       :foreground "#a8a8a8"))
    "My strike-through emphasis for Org.")

  (defface my-org-emphasis-strike-through
    '((((class color) (min-colors 88) (background light))
       :strike-through "#972500" :foreground "#505050")
      (((class color) (min-colors 88) (background dark))
       :strike-through "#ef8b50" :foreground "#a8a8a8"))
    "My strike-through emphasis for Org.")

  (setq org-emphasis-alist
        '(("*" my-org-emphasis-bold)
          ("/" my-org-emphasis-italic)
          ("_" my-org-emphasis-underline)
          ("=" org-verbatim verbatim)
          ("~" org-code verbatim)
          ("+" my-org-emphasis-strike-through))))

(use-package org-clock
  :after org
  :config
  (org-clock-persistence-insinuate)
  (setq org-clock-persist-file (expand-file-name "cache/org-clock-save.el" user-emacs-directory))
  (setq org-clock-history-length 23)
  (setq org-clock-in-resume t)
  (setq org-clock-into-drawer "LOGCLOCK")
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-clock-out-when-done t)
  (setq org-clock-persist t)
  (setq org-clock-clocktable-default-properties '(:maxlevel 5 :link t :tags t))
  (setq org-clock-persist-query-resume nil)
  (setq org-clock-report-include-clocking-task t))

(my/space-leader-def
  "oc" '(:ignore t :wk "Clock")
  "ocj" '(org-clock-goto :wk "Clock goto")
  "oci" '(org-clock-in :wk "Clock In")
  "oco" '(org-clock-out :wk "Clock Out")
  "ocl" '(org-clock-in-last :wk "Clock In Last"))

(setq bh/keep-clock-running nil)

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))
(defun bh/is-project-p ()
  "Any task with a todo keyword subtask."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))
(defun bh/find-project-task ()
  "Move point to the parent (project) task if any."
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))
(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
 Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))
(defun bh/clock-in-to-next (kw) ;; kw should not been deleted.
  "Switch a task from TODO to NEXT when clocking in.
 Skips capture tasks, projects, and subprojects.
 Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (bh/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-project-p))
      "TODO"))))
(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the selected task.
 If `ARG' is nil, set the Organization task
 as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-default-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
                                        ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-default-task-as-default)))))
(defun bh/punch-out ()
  "Punch out."
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))
(defun bh/clock-in-default-task ()
  "Clock In default task with specific ID."
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))
(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in."
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))
(defvar bh/default-task-id "20230307T170734.341755")
(defun bh/clock-in-default-task-as-default ()
  "Clock in default task."
  (interactive)
  (org-with-point-at (org-id-find bh/default-task-id 'marker)
    (org-clock-in '(16))))
(defun bh/clock-out-maybe ()
  "Clock out."
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

(defun bh/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one.
 Skip the default task and get the next one.
 A prefix `ARG' forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (widen)
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

(defun my/toggle-punch-in-or-out ()
  "Start clock or stop it when there is a clocking."
  (interactive)
  (if (org-clocking-p)
      (progn
        (bh/punch-out)
        (alert "Wish you have a good day!" :title "Punch Out"))
    (progn
      (bh/punch-in nil)
      (alert "Start Working: Fighting" :title "Punch In" ))))

(my/space-leader-def
  "op" '(my/toggle-punch-in-or-out :wk "Punch In or Out"))

(add-to-list 'display-buffer-alist
             '("\\*Org Note\\*"
               (display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 0.5)
               (window-parameters . ((no-other-window . t)
                                     (no-delete-other-windows . t)))))

(use-package ol
  :config
  (setq org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                               (vm-imap . vm-visit-imap-folder-other-frame)
                               (gnus . org-gnus-no-new-news)
                               (file . find-file)
                               (wl . wl-other-frame))))

;; https://200ok.ch/posts/2022-12-07_streamline_your_org_mode_workflow_with_automatic_clock_table_recalculation.html
;; Need add #+AUTOCALC_CLOCK_TABLES to org file.
(with-eval-after-load 'org
  (add-to-list 'org-options-keywords "AUTOCALC_CLOCK_TABLES:"))

(defun autocalc-clocktable ()
  "Auto update clock table."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char 0)
      (if (string-equal (car
                         (cdr
                          (car
                           (org-collect-keywords '("AUTOCALC_CLOCK_TABLES")))))
                        "t")
          (progn
            (goto-char (search-forward "clocktable"))
            (org-ctrl-c-ctrl-c))))))

(add-hook 'before-save-hook 'autocalc-clocktable)

(defun my/org-find-time-file-property (property &optional anywhere)
  "Return the position of the time file PROPERTY if it exists.
When ANYWHERE is non-nil, search beyond the preamble."
  (save-excursion
    (goto-char (point-min))
    (let ((first-heading
           (save-excursion
             (re-search-forward org-outline-regexp-bol nil t))))
      (when (re-search-forward (format "^#\\+%s:" property)
                               (if anywhere nil first-heading)
                               t)
        (point)))))

(defun my/org-set-time-file-property (property &optional anywhere pos)
  "Set the time file PROPERTY in the preamble.
When ANYWHERE is non-nil, search beyond the preamble.
If the position of the file PROPERTY has already been computed,
it can be passed in POS.

https://github.com/zaeph/.emacs.d/blob/615ac37be6bd78c37e967fdb43d28897a4116583/lisp/zp-org.el#L194"
  (when-let ((pos (or pos
                      (my/org-find-time-file-property property))))
    (save-excursion
      (goto-char pos)
      (if (looking-at-p " ")
          (forward-char)
        (insert " "))
      (delete-region (point) (line-end-position))
      (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
        (insert now)))))

(defun my/org-set-date ()
  "Update the LAST_MODIFIED file property in the preamble.
https://github.com/zaeph/.emacs.d/blob/615ac37be6bd78c37e967fdb43d28897a4116583/lisp/zp-org.el#L212"
  (when (and (derived-mode-p 'org-mode)
             (buffer-modified-p))
    (my/org-set-time-file-property "DATE")))

(add-hook 'before-save-hook 'my/org-set-date)

(use-package ekg
  :commands (ekg-show-notes-in-trash
             ekg-show-notes-for-today
             ekg-show-notes-with-tag
             ekg-show-notes-with-all-tags
             ekg-show-notes-with-any-tags
             ekg-show-rename-tag
             ekg-browse-url)
  :bind (("<f9>" . ekg-capture)
         ("C-<f9>" . ekg-capture-url)
         (:map ekg-notes-mode-map
               ("q" . quit-window)))
  :config
  (setq triples-default-database-filename (expand-file-name "ekg/triples.db" my-galaxy)))

(add-to-list 'display-buffer-alist '("\\*EKG"
                                     (display-buffer-pop-up-frame)
                                     (window-parameters
                                      (no-other-window . t)
                                      (mode-line-format . none)
                                      (no-delete-other-windows . t))))

(with-eval-after-load 'evil
  (evil-define-key '(normal visual) 'global
    "ged" 'ekg-show-notes-for-today
    "gee" 'ekg-show-notes-with-tag
    "gea" 'ekg-show-notes-with-any-tags
    "geA" 'ekg-show-notes-with-all-tags
    "geb" 'ekg-browse-url
    "ger" 'ekg-rename-tag)
  (evil-define-key 'normal ekg-notes-mode-map
    "A" 'ekg-notes-any-tags
    "B" 'ekg-notes-select-and-browse-url
    "a" 'ekg-notes-any-note-tags
    "b" 'ekg-notes-browse
    "c" 'ekg-notes-create
    "d" 'ekg-notes-delete
    "n" 'ekg-notes-next
    "o" 'ekg-notes-open
    "p" 'ekg-notes-previous
    "r" 'ekg-notes-remove
    "t" 'ekg-notes-tag
    "q" 'quit-window))

(use-package denote
  :commands (denote-signature denote-subdirectory denote-rename-file-using-front-matter
                              denote-rename-file
                              denote-link-or-create)
  :hook (dired-mode . denote-dired-mode)
  :config
  (setq denote-directory (expand-file-name "denote" my-galaxy)))

(with-eval-after-load 'evil
  (evil-define-key '(normal visual) 'global
    "gnbl" 'denote-org-dblock-insert-links
    "gnbb" 'denote-org-dblock-insert-backlinks
    "gns" 'denote-signature
    "gnd" 'denote-subdirectory
    "gnr" 'denote-rename-file-using-front-matter
    "gnR" 'denote-rename-file
    "gnl" 'denote-link-or-create))

(use-package denote-org-dblock
  :after denote org)

(use-package consult-notes
  :commands consult-notes
  :config
  (setq consult-notes-file-dir-sources
        `(("Articles"  ?a  ,(concat my-galaxy "/articles"))
          ("Denote Notes"  ?d ,(expand-file-name "denote" my-galaxy))
          ("Book Reading"  ?b ,(expand-file-name "denote/books" my-galaxy)))))
(defun my/new-article (article)
    (interactive "sTitle: ")
    (let ((filename (format "%s" article))
          (ext ".org"))
      (find-file (concat my-galaxy "/articles/" filename ext))
      (insert "#+TITLE: " article "\n")
      (tempel-insert 'hugo)))

(with-eval-after-load 'evil
  (evil-define-key '(normal visual) 'global
    "gnn" 'consult-notes
    "gna" 'my/new-article))

;; (use-package consult-notes-org-roam
;;   :commands consult-notes-org-roam-find-node-relation)

;; (my/space-leader-def
;;     "nv" '(consult-notes-org-roam-find-node-relation :wk "Node navigation"))

(use-package org-transclusion
  :commands (org-transclusion-make-from-link org-transclusion-add org-transclusion-add-all)
  :config
  (face-spec-set 'org-transclusion-fringe
                 '((((background light))
                    :foreground "black")
                   (t
                    :foreground "white"))
                 'face-override-spec)
  (face-spec-set 'org-transclusion-source-fringe
                 '((((background light))
                    :foreground "black")
                   (t
                    :foreground "white"))
                 'face-override-spec))
(my/space-leader-def
  "ot" '(:ignore t :wk "Transclusion")
  "ota" '(org-transclusion-add :wk "Add")
  "otA" '(org-transclusion-add-all :wk "Add all")
  "otr" '(org-transclusion-remove :wk "Remove")
  "otR" '(org-transclusion-remove-all :wk "Remove all")
  "otg" '(org-transclusion-refresh :wk "Refresh")
  "otm" '(org-transclusion-make-from-link :wk "Make link")
  "oto" '(org-transclusion-open-source :wk "Open source")
  "ote" '(org-transclusion-live-sync-start :wk "Edit live"))

(use-package org-agenda
  :bind ("C-<f12>" . org-agenda)
  :hook (org-agenda-finalize . #'org-agenda-find-same-or-today-or-agenda)
  :config
  (setq org-agenda-files (directory-files-recursively (expand-file-name "todos" my-galaxy) "org$"))
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-window-setup 'other-tab)
  (setq org-agenda-align-tags-to-column -120)

  (with-eval-after-load 'all-the-icons
    (setq org-agenda-category-icon-alist
          `(("\\`gtd\\'"
             (#(" " 0 1 (rear-nonsticky t display (raise 0.0)
                                         font-lock-face
                                         (:family "FontAwesome" :height 1.0)
                                         face
                                         (:family "FontAwesome" :height 1.0))))
             nil nil :ascent center)
            ("\\\cc\\\|[a-zA-z0-9]*"
             (#(" " 0 1 (rear-nonsticky t display (raise 0.0)
                                         font-lock-face
                                         (:family "FontAwesome" :height 1.0)
                                         face
                                         (:family "FontAwesome" :height 1.0))))
             nil nil :ascent center)))))

(with-eval-after-load 'evil
  (evil-set-initial-state 'org-agenda-mode 'motion)
  (evil-define-key 'motion org-agenda-mode-map
    (kbd "RET") 'org-agenda-switch-to
    "SPC" 'nil
    "gj" 'org-agenda-next-item
    "gr" 'org-agenda-redo
    "gR" 'org-agenda-redo-all
    "t" 'org-agenda-todo
    "u" 'org-agenda-undo
    "I" 'org-agenda-clock-in
    "O" 'org-agenda-clock-out
    "cg" 'org-agenda-clock-goto
    "cc" 'org-agenda-clock-cancel
    "cr" 'org-agenda-clockreport-mode))

(defun my/gtd-file ()
  (interactive)
  (find-file (expand-file-name "todos/org-gtd-tasks.org" my-galaxy)))

(use-package org-gtd
  :bind ("<f12>" . org-gtd-engage)
  :commands org-gtd-capture org-gtd-engage org-gtd-process-inbox org-gtd-show-all-next org-gtd-show-stuck-projects
  :init
  (setq org-gtd-update-ack "2.1.0")
  :custom
  (org-gtd-directory (expand-file-name "todos" my-galaxy))
  (org-agenda-property-list '("DELEGATED_TO"))
  (org-edna-use-inheritance t)
  :config
  (add-to-list 'org-agenda-files (expand-file-name "todos/org-gtd-tasks.org" my-galaxy))
  :bind (:map org-gtd-process-map
          ("C-c c" . org-gtd-choose)))

(my/space-leader-def
  "d" '(:ignore t :wk "Org gtd")
  "dp" '(org-gtd-process-inbox :wk "Process")
  "da" '(org-agenda-list :wk "Agenda list")
  "de" '(org-gtd-engage :wk "Engage")
  "dn" '(org-gtd-show-all-next :wk "Next tasks")
  "ds" '(org-gtd-show-stuck-projects :wk "Stuck projects"))

(use-package org-edna
  :after org-gtd
  :config
  (org-edna-load))

(use-package calendar
  :commands calendar
  :config
  (setq calendar-view-diary-initially-flag t)
  (setq calendar-mark-diary-entries-flag t)
  (setq calendar-mode-line-format nil)

  (setq calendar-date-style 'iso)
  (setq calendar-date-display-form calendar-iso-date-display-form)

  (setq calendar-time-display-form
        '(24-hours ":" minutes
                   (when time-zone
                     (format "(%s)" time-zone))))
  (setq diary-date-forms diary-iso-date-forms)
  :hook (calendar-today-visible . #'calendar-mark-today))

(my/space-leader-def
  "c" '(calendar :wk "Calendar"))

(use-package appt
  :after calendar
  :config
  (setq appt-display-diary nil)
  (setq appt-disp-window-function #'appt-disp-window)
  (setq appt-display-mode-line t)
  (setq appt-display-interval 3)
  (setq appt-audible nil)
  (setq appt-warning-time-regexp "appt \\([0-9]+\\)")
  (setq appt-message-warning-time 6)
  (add-hook 'diary-mode-hook #'appt-activate))

(use-package diary-lib
  :after calendar
  :config
  (add-hook 'diary-list-entries-hook #'diary-sort-entries)
  (add-hook 'diary-mode-hook #'goto-address-mode)
  (setq diary-display-function #'diary-fancy-display)
  (setq diary-header-line-format nil)
  (setq diary-list-include-blanks nil)
  (setq diary-abbreviated-year-flag nil)
  (setq diary-number-of-entries 7)
  (setq diary-comment-start ");;")
  (setq diary-comment-end "")
  (setq diary-nonmarking-symbol "!")

  (setq diary-file (expand-file-name "diary/diary.org" my-galaxy)))

(use-package olivetti
  :commands olivetti-mode)

(with-eval-after-load 'evil
  (evil-define-key 'normal org-mode-map
    "zw" 'olivetti-mode))

(use-package toc-org
  :hook (org-mode . toc-org-mode))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("❶" "❷" "❸" "❹" "❺" "❻" "❼"))
  ;; (setq org-superstar-headline-bullets-list '("1" "2" "3" "4" "5" "6" "7"))
  ;; (setq org-superstar-headline-bullets-list '("①" "②" "③" "④" "⑤" "⑥"))
  (setq org-hide-leading-stars t))

(use-package org-download
  :commands org-download-enable
  :hook (org-mode . org-download-enable)
  :init
  (setq org-download-image-dir (expand-file-name "pictures" my-galaxy))
  (setq org-download-heading-lvl nil)
  :config
  (setq org-download-screenshot-method 'screencapture)
  (setq org-download-abbreviate-filename-function 'expand-file-name)
  (setq org-download-timestamp "%Y%m%d%H%M%S")
  (setq org-download-display-inline-images nil)
  (setq org-download-annotate-function (lambda (_link) ""))
  (setq org-download-image-attr-list '("#+NAME: fig: "
                                       "#+CAPTION: "
                                       "#+ATTR_ORG: :width 500px"
                                       "#+ATTR_LATEX: :width 10cm :placement [!htpb]"
                                       "#+ATTR_HTML: :width 600px"))
  (defun my/auto-change-file-paths ()
    (interactive)
    (save-excursion
      (previous-line)
      (while (re-search-forward (expand-file-name "~") nil t)
        (replace-match "~" t nil))))
  (advice-add 'org-download-clipboard :after 'my/auto-change-file-paths))
(my/space-leader-def
  "od" '(:ignore t :wk "Download")
  "odc" '(org-download-clipboard :wk "Download Clipboard")
  "ody" '(org-download-yank :wk "Download Yank")
  "odr" '(org-download-rename-last-file :wk "Rename last file")
  "odR" '(org-download-rename-at-point :wk "Rename point"))

(use-package org-appear
  :config
  (setq org-appear-autolinks t)
  (setq org-appear-trigger 'manual)
  :hook ((org-mode . (lambda ()
                       (add-hook 'evil-insert-state-entry-hook
                                 #'org-appear-manual-start
                                 nil
                                 t)
                       (add-hook 'evil-insert-state-exit-hook
                                 #'org-appear-manual-stop
                                 nil
                                 t)))
         (org-mode . org-appear-mode)))

(use-package math-preview
  :commands (math-preview-all math-preview-at-point)
  :config
  (setq math-preview-scale 1.1)
  (setq math-preview-raise 0.3)
  (setq math-preview-margin '(1 . 0)))

(my/space-leader-def
  "p" '(:ignore t :wk "Preview")
  "pa" '(math-preview-all :wk "All")
  "pA" '(math-preview-clear-all :wk "Clear All")
  "pp" '(math-preview-at-point :wk "Point")
  "pP" '(math-preview-clear-at-point :wk "Clear Point")
  "pr" '(math-preview-region :wk "Region")
  "pR" '(math-preview-clear-region :wk "Clear Region"))

(use-package plantuml
  :commands (plantuml-org-to-mindmap-open plantuml-org-to-wbs-open)
  :config
  (setq plantuml-jar-path
        (concat (string-trim
                 (shell-command-to-string "readlink -f $(brew --prefix plantuml)"))
                "/libexec/plantuml.jar")))
(my/space-leader-def
  "pm" '(plantuml-org-to-mindmap-open :wk "Mindmap")
  "ps" '(plantuml-org-to-wbs-open :wk "Work Breakdown Structure"))

(use-package org-rainbow-tags
  :hook (org-mode . org-rainbow-tags-mode))

(use-package alarm-clock
  :commands (alarm-clock-set alarm-clock-list-view)
  :config
  (setq alarm-clock-cache-file (expand-file-name "var/.alarm-clock.cache" user-emacs-directory)))

(use-package alert
  :commands alert
  :config
  (setq alert-default-style 'osx-notifier))

(use-package pomm
  :commands pomm
  :config
  (setq pomm-state-file-location (expand-file-name "cache/pomm" user-emacs-directory))
  (pomm-mode-line-mode 1))

(defun org-export-docx ()
  "Convert org to docx."
  (interactive)
  (let ((docx-file (concat (file-name-sans-extension (buffer-file-name)) ".docx"))
        (template-file (expand-file-name "template/template.docx" user-emacs-directory)))
    (shell-command (format "pandoc %s -o %s --reference-doc=%s" (buffer-file-name) docx-file template-file))
    (message "Convert finish: %s" docx-file)))

(use-package beancount
  :mode (".bean" . beancount-mode)
  :hook ((beancount-mode . (lambda ()
                             (setq-local electric-indent-chars nil)))
         (beancount-mode . outline-minor-mode))
  :config
  (evil-define-key 'normal 'beancount-mode-map
    "zf" 'beancount-fava)

  ;; insert whole transaction instead of only insert date.
  (defun my/beancount-insert-transaction (&optional days)
    "Start a new timestamped directive with date shifted by DAYS from today."
    (interactive "P")
    (unless (bolp) (newline))
    (insert (beancount--shift-current-date days) " * \"\" \"\"")
    (evil-backward-char 3)
    (evil-insert 0))

  (advice-add 'beancount-insert-date :override 'my/beancount-insert-transaction)

  ;; Auto open browser after beancount-fava started.
  (defun my/browser-beancount-fava ()
    (if beancount--fava-process
        (browse-url "http://127.0.0.1:5000")))

  (advice-add 'beancount-fava :after 'my/browser-beancount-fava)

  ;; auto align transaction before save file.
  (defun my/beancount-align-transaction ()
    "Align visible region in current buffer."
    (save-excursion
      (indent-region (window-start) (window-end))))

  (add-hook 'before-save-hook (lambda ()
                                (if (eq major-mode 'beancount-mode)
                                    (my/beancount-align-transaction))))

  ;; If cursor in "", activate input method rime.
  (defun my/beancount-activate-input-method ()
    (when (eq major-mode 'beancount-mode)
      (if (not (bounds-of-thing-at-point 'whitespace))
          (if (bounds-of-thing-at-point 'string)
              (activate-input-method "rime")))))

  (add-hook 'evil-insert-state-entry-hook #'my/beancount-activate-input-method))

(use-package oc
  :after org
  :config
  (setq org-cite-global-bibliography `(,(concat my-galaxy "/bibtexs/References.bib"))))

(use-package citar
  :commands citar-open citar-open-entry citar-open-files citar-open-notes citar-open-links
  :config
  (setq citar-bibliography org-cite-global-bibliography)
  (setq citar-notes-paths `(,(expand-file-name "references" my-galaxy)))
  (setq citar-library-file-extensions '("pdf" "jpg" "epub"))
  (setq citar-templates '((main . "${author editor:30} ${date year issued:4} ${title:48}")
                          (suffix . "${=key= id:15} ${=type=:12} ${tags keywords:*}")
                          (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
                          (note . "${title}")))
  (setq citar-symbol-separator "  ")
  (setq citar-file-additional-files-separator "-")
  (setq citar-at-point-function 'embark-act)
  (with-eval-after-load 'all-the-icons
   (setq citar-symbols
        `((file ,(all-the-icons-faicon "file-pdf-o" :face 'all-the-icons-dred :v-adjust -0.1) . " ")
          (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
          (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))))

(use-package citar-latex
  :after citar)

(use-package citar-capf
  :hook ((LaTeX-mode . citar-capf-setup)
         (org-mode . citar-capf-setup)))

(use-package citar-org
  :after citar
  :config
  (setq org-cite-insert-processor 'citar)
  (setq org-cite-follow-processor 'citar)
  (setq org-cite-activate-processor 'citar)

  (with-eval-after-load 'citar-org
    (define-key citar-org-citation-map (kbd "RET") 'org-open-at-point)))

(use-package citar-embark
  :commands citar-embark-mode
  :hook (org-mode . citar-embark-mode))

(my/space-leader-def
  "re" '(citar-open-entry :wk "Open entry")
  "rp" '(citar-open-files :wk "Open files")
  "ri" '(citar-insert-citation :wk "Insert citation")
  "rn" '(citar-open-notes :wk "Open/Create note")
  "rl" '(citar-open-links :wk "Open links"))

(use-package citar-denote
  :after citar
  :config
  (defun my/citar-open-notes (citekeys)
    "Open notes associated with the CITEKEYS."
    (interactive (list (citar-select-refs :filter (citar-denote-has-notes))))
    (pcase (let ((embark-default-action-overrides
                  (cons (cons t #'citar--open-resource)
                        (bound-and-true-p embark-default-action-overrides))))
             (citar--select-resource citekeys :notes t :create-notes t))
      (`(note . ,note) (citar-open-note note))
      (`(create-note . ,citekey) (citar-create-note citekey))))
  (advice-add 'citar-denote-open-note :override 'my/citar-open-notes))

(use-package bibtex-completion
  :after org-roam-bibtex
  :config
  (setq bibtex-completion-bibliography org-cite-global-bibliography)
  (setq bibtex-completion-notes-path (expand-file-name "roam/ref" my-galaxy))
  (setq bibtex-completion-pdf-field "File")
  (setq bibtex-completion-additional-search-fields '(keywords journal booktitle))
  (setq bibtex-completion-pdf-symbol "P")
  (setq bibtex-completion-notes-symbol "N")
  (setq bibtex-completion-display-formats '((article . "${=has-pdf=:1} ${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
                                            (inbook . "${=has-pdf=:1} ${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
                                            (incollection . "${=has-pdf=:1} ${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
                                            (inproceedings . "${=has-pdf=:1} ${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
                                            (t . "${=has-pdf=:1} ${=has-note=:1} ${year:4} ${author:36} ${title:*}"))))

(use-package ebib
  :bind ("<f2>" . ebib)
  :config
  (setq ebib-preload-bib-files org-cite-global-bibliography)

  (setq ebib-keywords (concat org-roam-directory "/bibtexs/keywords.txt"))
  (setq ebib-notes-directory (concat org-roam-directory "/ref"))
  (setq ebib-filters-default-file (concat org-roam-directory "/bibtexs/ebib-filters"))
  (setq ebib-reading-list-file (concat org-roam-directory "/bibtexs/reading_list.org"))

  (setq ebib-keywords-field-keep-sorted t)
  (setq ebib-keywords-file-save-on-exit 'always)

  (setq ebib-index-columns
        '(("Entry Key" 30 t) ("Note" 1 nil) ("Year" 6 t) ("Title" 50 t)))
  (setq ebib-file-associations '(("ps" . "gv"))))

(use-package scihub
  :commands scihub
  :config
  (setq scihub-download-directory "~/Downloads/")
  (setq scihub-open-after-download t)
  (setq scihub-fetch-domain 'scihub-fetch-domains-lovescihub))

(use-package biblio
  :commands biblio-lookup
  :general
  (:states 'normal
           "SPC rd" 'my/biblio-lookup-crossref :no-autoload t)
  :preface
  (defun my/biblio-lookup-crossref ()
    (interactive)
    (biblio-lookup 'biblio-crossref-backend)))

(my/space-leader-def
  "rd" '(my/biblio-lookup-crossref :wk "Get bib from crossfer"))

(use-package tex
  :mode (".tex" . LaTeX-mode)
  :init
  (load "auctex.el" nil t t)
  (load "preview-latex.el" nil t t)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-save-query nil)
  (setq TeX-electric-sub-and-superscript t)
  (setq TeX-auto-local ".auctex-auto")
  (setq TeX-style-local ".auctex-style")
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-source-correlate-start-server nil)

  (setq-default TeX-master t)
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
  (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))
  (add-to-list 'TeX-view-program-list '("PDF Tools" TeX-pdf-tools-sync-view))
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(use-package latex
  :bind (:map LaTeX-mode-map
              ("C-c h" . TeX-doc)))

(use-package font-latex
  :after tex
  :config
  (setq font-latex-match-reference-keywords
        '(;; BibLaTeX.
          ("printbibliography" "[{")
          ("addbibresource" "[{")
          ;; Standard commands.
          ("cite" "[{")
          ("citep" "[{")
          ("citet" "[{")
          ("Cite" "[{")
          ("parencite" "[{")
          ("Parencite" "[{")
          ("footcite" "[{")
          ("footcitetext" "[{")
          ;; Style-specific commands.
          ("textcite" "[{")
          ("Textcite" "[{")
          ("smartcite" "[{")
          ("Smartcite" "[{")
          ("cite*" "[{")
          ("parencite*" "[{")
          ("supercite" "[{")
          ;; Qualified citation lists.
          ("cites" "[{")
          ("Cites" "[{")
          ("parencites" "[{")
          ("Parencites" "[{")
          ("footcites" "[{")
          ("footcitetexts" "[{")
          ("smartcites" "[{")
          ("Smartcites" "[{")
          ("textcites" "[{")
          ("Textcites" "[{")
          ("supercites" "[{")
          ;; Style-independent commands.
          ("autocite" "[{")
          ("Autocite" "[{")
          ("autocite*" "[{")
          ("Autocite*" "[{")
          ("autocites" "[{")
          ("Autocites" "[{")
          ;; Text commands.
          ("citeauthor" "[{")
          ("Citeauthor" "[{")
          ("citetitle" "[{")
          ("citetitle*" "[{")
          ("citeyear" "[{")
          ("citedate" "[{")
          ("citeurl" "[{")
          ;; Special commands.
          ("fullcite" "[{")
          ;; Cleveref.
          ("cref" "{")
          ("Cref" "{")
          ("cpageref" "{")
          ("Cpageref" "{")
          ("cpagerefrange" "{")
          ("Cpagerefrange" "{")
          ("crefrange" "{")
          ("Crefrange" "{")
          ("labelcref" "{")))

  (setq font-latex-match-textual-keywords
        '(;; BibLaTeX brackets.
          ("parentext" "{")
          ("brackettext" "{")
          ("hybridblockquote" "[{")
          ;; Auxiliary commands.
          ("textelp" "{")
          ("textelp*" "{")
          ("textins" "{")
          ("textins*" "{")
          ;; Subcaption.
          ("subcaption" "[{")))

  (setq font-latex-match-variable-keywords
        '(;; Amsmath.
          ("numberwithin" "{")
          ;; Enumitem.
          ("setlist" "[{")
          ("setlist*" "[{")
          ("newlist" "{")
          ("renewlist" "{")
          ("setlistdepth" "{")
          ("restartlist" "{")
          ("crefname" "{"))))

(use-package cdlatex
  :hook ((LaTeX-mode . turn-on-cdlatex)
         (org-mode . org-cdlatex-mode)))

(use-package reftex
  :hook ((LaTeX-mode . turn-on-reftex)
         (reftex-toc-mode . menu-bar--visual-line-mode-enable))
  :config
  (setq reftex-toc-split-windows-horizontally t)
  (setq reftex-toc-split-windows-fraction 0.25))

(add-to-list 'display-buffer-alist '("\\*toc\\*"
                                     (display-buffer-reuse-window)
                                     (side . left)
                                     (window-parameters
                                      (mode-line-format . none)
                                      (delete-other-windows . t))))

(use-package org
  :config
  (setq org-highlight-latex-and-related '(latex script)))

(use-package ox-latex
  :defer 3
  :config
  (setq org-latex-src-block-backend 'minted)
  (setq org-latex-minted-options '(("breaklines" "true")
                                   ("breakanywhere" "true")))

  (setq org-latex-classes nil)
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

  (add-to-list 'org-latex-classes '("article-cn" "\\documentclass{ctexart}
                                      [NO-DEFAULT-PACKAGES]
                                      [NO-PACKAGES]
                                      [EXTRA]"
                                    ("\\section{%s}" . "\\section*{%s}")
                                    ("\\subsection{%s}" . "\\subsection*{%s}")
                                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                    ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes '("article" "\\documentclass[11pt]{article}
                                      [NO-DEFAULT-PACKAGES]
                                      [NO-PACKAGES]
                                      [EXTRA]"
                                    ("\\section{%s}" . "\\section*{%s}")
                                    ("\\subsection{%s}" . "\\subsection*{%s}")
                                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                    ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes '("beamer" "\\documentclass[presentation]{beamer}
                                      [DEFAULT-PACKAGES]
                                      [PACKAGES]
                                      [EXTRA]"
                                    ("\\section{%s}" . "\\section*{%s}")
                                    ("\\subsection{%s}" . "\\subsection*{%s}")
                                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

  (setq org-latex-pdf-process
        '("xelatex -8bit --shell-escape  -interaction=nonstopmode -output-directory %o %f"
          "bibtex -shell-escape %b"
          "xelatex -8bit --shell-escape  -interaction=nonstopmode -output-directory %o %f"
          "xelatex -8bit --shell-escape  -interaction=nonstopmode -output-directory %o %f"
          "rm -fr %b.out %b.log %b.tex %b.brf %b.bbl"))

  (setq org-latex-logfiles-extensions '("lof" "lot" "tex~" "aux" "idx" "log"
                                        "out" "toc" "nav" "snm" "vrb" "dvi"
                                        "fdb_latexmk" "blg" "brf" "fls"
                                        "entoc" "ps" "spl" "bbl"))

  (setq org-latex-prefer-user-labels t))

(use-package auctex-latexmk
  :hook (LaTeX-mode . auctex-latexmk-setup))

(use-package ox-beamer
  :defer 3
  :after org)

(use-package ox-hugo
  :defer 3
  :after ox)

(use-package ox-html
  :after ox
  :config
  (setq org-html-preamble t)
  (setq org-html-preamble-format
        '(("en" "<a href=\"/index.html\" class=\"button\">Home</a>
               <a href=\"/notes/index.html\" class=\"button\">Notes</a>
               <a href=\"/engineering/index.html\" class=\"button\">Engineering</a>
               <a href=\"/movies/index.html\" class=\"button\">Movies</a>
               <a href=\"/books/index.html\" class=\"button\">Books</a>
               <a href=\"/about.html\" class=\"button\">About</a>
               <hr>")))

  (setq org-html-postamble t)

  (setq org-html-postamble-format
        '(("en" "<hr><div class=\"generated\">Created with %c on MacOS</div>")))

  (setq org-html-head-include-default-style nil)

  (setq org-html-head
        "<link rel=\"stylesheet\" type=\"text/css\" href=\"../css/style.css\" />"))

(use-package ox-publish
  :after ox
  :config
  (defvar my/publish-directory "~/shuyi.github.io")

  (setq org-publish-project-alist
        `(("site"
           :base-directory ,website-directory
           :base-extension "org"
           :recursive nil
           :publishing-directory ,my/publish-directory
           :publishing-function org-html-publish-to-html)

          ("notes"
           :base-directory ,(expand-file-name "notes" website-directory)
           :base-extension "org"
           :publishing-directory ,(expand-file-name "notes" my/publish-directory)
           :publishing-function org-html-publish-to-html
           :auto-sitemap t
           :sitemap-filename "index.org"
           :sitemap-title "Notes"
           :sitemap-sort-files anti-chronologically)
          ("books"
           :base-directory ,(expand-file-name "books" website-directory)
           :base-extension "org"
           :publishing-directory ,(expand-file-name "books" my/publish-directory)
           :publishing-function org-html-publish-to-html
           :auto-sitemap t
           :sitemap-filename "index.org"
           :sitemap-title "Books"
           :sitemap-sort-files anti-chronologically)
          ("movies"
           :base-directory ,(expand-file-name "movies" website-directory)
           :base-extension "org"
           :publishing-directory ,(expand-file-name "movies" my/publish-directory)
           :publishing-function org-html-publish-to-html
           :auto-sitemap t
           :sitemap-filename "index.org"
           :sitemap-title "Movies"
           :sitemap-sort-files anti-chronologically)
          ("engineering"
           :base-directory ,(expand-file-name "engineering" website-directory)
           :base-extension "org"
           :publishing-directory ,(expand-file-name "engineering" my/publish-directory)
           :publishing-function org-html-publish-to-html
           :auto-sitemap t
           :sitemap-filename "index.org"
           :sitemap-title "Engineering"
           :sitemap-sort-files anti-chronologically)
          ("static"
           :base-directory ,website-directory
           :base-extension "css\\|txt\\|jpg\\|gif\\|png"
           :recursive t
           :publishing-directory  ,my/publish-directory
           :publishing-function org-publish-attachment)

          ("personal-website" :components ("site" "notes" "books"
                                           "movies" "engineering" "static")))))

;; eww
;; Install readability first.
;; npm install -g readability-cli
;; (setq eww-retrieve-command '("readable"))

;; Another choice `websearch'.
;; Search engine
(use-package engine-mode
  :hook (on-first-input . engine-mode)
  :config
  (defengine google "https://google.com/search?q=%s"
             :keybinding "g"
             :docstring "Search Google.")
  (defengine wikipedia "https://en.wikipedia.org/wiki/Special:Search?search=%s"
             :keybinding "w"
             :docstring "Search Wikipedia.")
  (defengine github "https://github.com/search?ref=simplesearch&q=%s"
             :keybinding "h"
             :docstring "Search GitHub.")
  (defengine youtube "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
             :keybinding "y"
             :docstring "Search YouTube.")
  (defengine moviedouban "https://search.douban.com/movie/subject_search?search_text=%s"
             :keybinding "m"
             :docstring "Search Moive DouBan.")
  (defengine bookdouban "https://search.douban.com/book/subject_search?search_text=%s"
             :keybinding "b"
             :docstring "Search Book DouBan.")
  (defengine zhihu "https://www.zhihu.com/search?type=content&q=%s"
             :keybinding "z"
             :docstring "Search Zhihu."))

(my/space-leader-def
  "s" '(:ignore t :wk "Search")
  "sb" '(engine/search-bookdouban :wk "Book")
  "ss" '(engine/search-google :wk "Google")
  "sg" '(engine/search-github :wk "Github")
  "sw" '(engine/search-wikipedia :wk "Wiki")
  "sm" '(engine/search-moviedouban :wk "Movie")
  "sz" '(engine/search-zhihu :wk "Zhihu")
  "sr" '(rg :wk "rg")
  "sl" '(consult-git-grep :wk "git"))

(use-package grab-mac-link
  :commands grab-mac-link-dwim
  :preface
  (defun my/link-grab ()
    (interactive)
    (grab-mac-link-dwim 'safari))
  :bind ("<f8>" . my/link-grab))

(use-package elfeed
  :defer t
  :preface
  (defun elfeed-display-buffer (buf &optional act)
    (pop-to-buffer buf '((display-buffer-reuse-window display-buffer-in-side-window)
                         (side . bottom)
                         (window-height . 0.8)
                         (reusable-frames . visible)
                         (window-parameters
                          (select . t)
                          (quit . t)
                          (popup . t)))))
  :config
  (setq elfeed-show-entry-switch #'elfeed-display-buffer))

(add-to-list 'display-buffer-alist
             `(,(rx (| "*elfeed-search*"
                       "*elfeed-summary*"
                       "*elfeed-entry-"))
               (display-buffer-in-tab)
               (tab-name . "RSS")
               (tab-group . "RSS")
               (window-parameters . ((mode-line-format . none)))))

(use-package elfeed-org
  :after elfeed)

(setq rmh-elfeed-org-files `(,(concat my-galaxy "/rss/elfeed.org")))

(defun my/rss-source ()
    "Open elfeed config file."
    (interactive)
    (find-file (car rmh-elfeed-org-files)))

(my/space-leader-def
  "fe" '(my/rss-source :wk "Elfeed file"))

(use-package elfeed-summary
  :commands elfeed-summary
  :config
  (setq elfeed-summary-other-window t)
  (setq elfeed-summary-settings
        '((group (:title . "科技")
                 (:elements (query . (and tec (not emacs) (not blogs)))
                            (group (:title . "Emacs")
                                   (:elements (query . emacs))
                                   (:face . org-level-1))
                            (group (:title . "Blogs")
                                   (:elements (query . blogs)))))
          (group (:title . "News")
                 (:elements (query . news)))
          (group (:title . "Books")
                 (:elements (query . book)))
          (group (:title . "Finance")
                 (:elements (query . finance)))
          (group (:title . "Youtube")
                 (:elements (query . video)))))

  (advice-add 'elfeed-summary :after 'elfeed-summary-update)
  (advice-add 'elfeed-summary :before 'elfeed-org))

(my/space-leader-def
  "E" '(elfeed-summary :wk "Elfeed"))

(use-package pdf-tools
  :hook ((doc-view-mode . pdf-tools-install)
         (dirvish-setup . pdf-tools-install)
         (pdf-tools-enabled . pdf-view-themed-minor-mode)))

(use-package pdf-view
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install t nil t nil)
  (evil-declare-key 'normal pdf-view-mode-map
    "gh" 'pdf-annot-add-highlight-markup-annotation
    "ga" 'pdf-annot-add-text-annotation
    "gd" 'pdf-annot-delete)
  :init
  (setq pdf-view-display-size 'fit-width)
  (setq pdf-view-use-unicode-ligther nil)
  (setq pdf-view-use-scaling t)
  (setq pdf-view-use-imagemagick nil)
  (setq pdf-annot-activate-created-annotations nil))

(add-to-list 'display-buffer-alist '("\\.pdf"
                                     (display-buffer-in-tab)
                                     (tab-name . "PDF")
                                     (tab-group . "PDF")))
(use-package pdf-occur
  :hook (pdf-view-mode . pdf-occur-global-minor-mode))

(use-package pdf-history
  :hook (pdf-view-mode . pdf-history-minor-mode))

(use-package pdf-links
  :hook (pdf-view-mode . pdf-links-minor-mode))

(defun my/get-file-name ()
  "Copy pdf file name."
  (interactive)
  (kill-new (file-name-base (buffer-file-name)))
  (message "Copied %s" (file-name-base (buffer-file-name))))

(use-package pdf-outline
  :hook (pdf-view-mode . pdf-outline-minor-mode)
  :bind (:map pdf-outline-buffer-mode-map
              ("RET" . pdf-outline-follow-link-and-quit)))

(add-to-list 'display-buffer-alist '("\\*Outline"
                                     (display-buffer-in-side-window)
                                     (side . right)
                                     (window-width . 0.5)
                                     (window-parameters
                                      (mode-line-format . none))))
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
      (pdf-misc-print-document filename)))
  :bind (:map pdf-view-mode-map
              ([remap pdf-misc-print-document] . mrb/pdf-misc-print-pages)))

(defun my/pdf-extract-highlight ()
  "Extract highlight to plain text.
When it finised, it will jump to note file."
  (interactive)
  (let* ((pdf-filename (buffer-name))
         (txt-filename (make-temp-file "/tmp/annot-"))
         (org-file (read-file-name "Save extracted highlights to org file: " (expand-file-name "references/" my-galaxy))))
    (async-start
     `(lambda ()
        (shell-command-to-string (format "/opt/homebrew/bin/python3.10 ~/pdfannots/pdfannots.py \"%s\""
                                          ,pdf-filename)))
     `(lambda (annot)
        (setq annot (replace-regexp-in-string "##" "*" annot))
        (with-temp-buffer
          (insert annot)
          (write-region (point-min) (point-max) ,org-file t))
        (find-file ,org-file)))))

(my/space-leader-def
  "mh" '(my/pdf-extract-highlight :wk "Extract highlight"))

(defun my/dired-pdf-to-png ()
  (interactive)
  (let* ((filename (dired-get-filename)))
    (if (string-match "\.pdf" filename)
        (let* ((pdf-base-name (file-name-sans-extension (file-name-nondirectory filename)))
               (png (concat pdf-base-name ".png"))
               (pdf-info (shell-command-to-string (format "pdfinfo %s | grep Pages | awk '{print $2}'" filename)))
               (pdf-pages (string-to-number pdf-info)))
          (when (file-exists-p png)
            (delete-file png))
          (if (= pdf-pages 1)
              (start-process-shell-command
               "pdf-to-png"
               nil
               (format "pdftoppm -singlefile -r 600 %s %s -png" filename pdf-base-name))
            (start-process-shell-command
             "pdf-to-png"
             nil
             (format "pdftoppm -r 600 %s %s -png" filename pdf-base-name))))
      (message "Current file is not a PDF file."))))

(use-package nov
  :mode (".epub" . nov-mode)
  :config
  (setq nov-unzip-program (executable-find "bsdtar")
        nov-unzip-args '("-xC" directory "-f" filename)))

;; Export catalog need to use argument: set entry type to mixed, default is book.
(use-package calibredb
  :bind (("<f1>" . calibredb)
         :map calibredb-search-mode-map
         ("C-c l" . calibredb-copy-as-org-link))
  :config
  (setq calibredb-root-dir "~/Nextcloud/L.Calibre/")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-add-delete-original-file "yes")
  (setq calibredb-size-show t)
  (setq calibredb-format-character-icons t)

  (setq calibredb-ref-default-bibliography (expand-file-name "calibre.bib" calibredb-root-dir))

  (evil-set-initial-state 'calibredb-search-mode 'emacs))

(use-package vterm
  :bind ("C-c , v" . toggle-vterm)
  :config
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 5000)
  (add-to-list 'display-buffer-alist
               '("\\*vterm\\*"
                 (display-buffer-in-side-window)
                 (window-height . 0.35)
                 (side . bottom)
                 (slot . -1)))
  (defun toggle-vterm ()
    "Toggle vterm on or off."
    (interactive)
    (if (get-buffer-window "*vterm*")
        (delete-window (get-buffer-window "*vterm*"))
      (progn
        (vterm)
        (evil-insert 1)))))

(my/space-leader-def
  "v" '(toggle-vterm :wk "vterm"))

(setq user-full-name "Duan Ning")
(setq user-mail-address "duan_n@outlook.com")

(use-package simple
  :config
  (setq mail-user-agent 'mu4e-user-agent))

(use-package message
  :config
(setq message-sendmail-envelope-from 'header)
(setq message-kill-buffer-query nil)
(setq message-sendmail-extra-arguments '("-a" "outlook"))
(setq message-send-mail-function 'sendmail-send-it))

(add-to-list 'load-path "/opt/homebrew/opt/mu/share/emacs/site-lisp/mu/mu4e")
(unless (fboundp 'mu4e)
  (autoload #'mu4e "mu4e" nil t))

(run-with-idle-timer 5 nil #'(lambda () (mu4e 'background)))

(with-eval-after-load 'mu4e
  (setq mu4e-mu-binary (executable-find "mu"))
  (setq mu4e-update-interval (* 15 60))
  (setq mu4e-attachment-dir "~/Downloads/")
  (setq mu4e-get-mail-command (concat (executable-find "mbsync") " -a"))
  (setq mu4e-index-update-in-background t)
  (setq mu4e-index-update-error-warning t)
  (setq mu4e-index-update-error-warning nil)
  (setq mu4e-index-cleanup t)
  (setq mu4e-view-show-images t)
  (setq mu4e-view-image-max-width 800)
  (setq mu4e-view-show-addresses t)
  (setq mu4e-confirm-quit nil)
  (setq mu4e-context-policy 'pick-first)
  (with-eval-after-load 'mu4e
    (setq mu4e-sent-folder   "/outlook/Sent"
          mu4e-drafts-folder "/outlook/Drafts"
          mu4e-trash-folder  "/outlook/Deleted"
          mu4e-refile-folder  "/outlook/Archive"))
  (setq mu4e-view-prefer-html nil)
  (setq mu4e-html2text-command 'mu4e-shr2text)
  (setq mu4e-main-hide-personal-addresses t)
  (setq mu4e-headers-precise-alignment t)
  (setq mu4e-headers-include-related t)
  (setq mu4e-headers-auto-update t)
  (setq mu4e-headers-date-format "%d/%m/%y")
  (setq mu4e-headers-time-format "%H:%M")
  (setq mu4e-headers-fields '((:flags . 4)
                              (:human-date . 9)
                              (:subject . 90)
                              (:from-or-to . 40)
                              (:tags . 20)))
  (setq mu4e-bookmarks '(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
                         ("date:today..now" "Today's messages" ?t)
                         ("date:7d..now" "Last 7 days" ?w)
                         ("date:1d..now AND NOT list:emacs-orgmode.gnu.org" "Last 1 days" ?o)
                         ("date:1d..now AND list:emacs-orgmode.gnu.org" "Last 1 days (org mode)" ?m)
                         ("maildir:/drafts" "drafts" ?d)
                         ("flag:flagged AND NOT flag:trashed" "flagged" ?f)
                         ("mime:image/*" "Messages with images" ?p)))
  (setq mu4e-compose-reply-ignore-address '("no-?reply" "duan_n@outlook.com"))
  (setq mu4e-compose-format-flowed nil)
  (setq mu4e-compose-signature-auto-include nil)
  (setq mu4e-compose-dont-reply-to-self t))

(with-eval-after-load 'mu4e
  (define-key mu4e-headers-mode-map (kbd "C-c l") 'org-store-link))

(my/space-leader-def
  "e" '(mu4e :wk "MAIL"))

(add-to-list 'display-buffer-alist
             `(,(rx (| "*mu4e-main*"
                       "*mu4e-headers*"))
               (display-buffer-in-tab)
               (tab-name . "Mail")
               (tab-group . "Mail")
               (window-parameters . ((mode-line-format . none)))))

(defun extra-email-to-pdf (msg &optional args)
  "Pdf temp file MSG to a new name with ARGS ignored."
  (let* ((async-shell-command-display-buffer nil)
         (temp (format-time-string (expand-file-name "%Y-%m-%dT%H:%M.pdf" mail-source-directory)))
         (name (read-string "File name: " temp))
         (html (replace-regexp-in-string (regexp-quote "file://") "" msg t t)))
    (if args (message "Additional optional argument was ignored when saving to PDF."))
    (async-shell-command (concat "pandoc " html " -o " name))))

(defun extra-print-email-to-pdf (msg &optional skip-headers)
  "Save current MSG as a pdf if it includes an HTML-part.
If SKIP-HEADERS is set, do not show include message headers."
  (let* ((browse-url-browser-function  'extra-email-to-pdf))
    (mu4e-action-view-in-browser msg skip-headers)))
(with-eval-after-load 'mu4e
  (add-to-list 'mu4e-view-actions '("print to PDF"  . extra-print-email-to-pdf)))

(defun extra-move-temp-email-location (msg &optional args)
  "Move and rename temp file MSG to a new location with ARGS ignored."
  (let* ((temp (format-time-string (expand-file-name "html/%Y-%m-%dT%H:%M.html" mail-source-directory)))
         (name (read-string "File name: " temp))
         (file (replace-regexp-in-string (regexp-quote "file://") "" msg t t)))
    (if args (message "Additional optional argument was ignored when saving to HTML."))
    (rename-file file name)))

(defun extra-save-email-html (msg &optional skip-headers)
  "Save current MSG HTML-part.
If SKIP-HEADERS is set, do not show include message headers."
  (let* ((extra-temp-email-dir (expand-file-name "html" mail-source-directory))
         (browse-url-browser-function  'extra-move-temp-email-location))
    (mu4e-action-view-in-browser msg skip-headers)))

(with-eval-after-load 'mu4e
  (add-to-list 'mu4e-view-actions '("download as html"  . extra-save-email-html)))

(with-eval-after-load 'all-the-icons
  (setq display-time-mail-icon `(,(propertize
                                   (all-the-icons-material "mail")
                                   'face `(:family ,(all-the-icons-material-family))))))

(with-eval-after-load 'mu4e
  (setq mu4e-use-fancy-chars nil))

(use-package mu4e-alert
  :after mu4e
  :config
  (mu4e-alert-set-default-style 'osx-notifier)
  (mu4e-alert-enable-notifications)
  (mu4e-alert-enable-mode-line-display))

(use-package mu4e-column-faces
  :hook (mu4e-main-mode . mu4e-column-faces-mode))

(add-hook 'mu4e-main-mode-hook
          (lambda ()
            (progn
              (require 'smtpmail-async)
              (setq send-mail-function 'async-sendmail-send-it)
              (setq message-send-mail-function 'async-smtpmail-send-it))))
(setq sendmail-program (executable-find "msmtp"))
(setq mail-specify-envelope-from t)
(setq mail-envelope-from 'header)

(use-package org-msg
  :hook (mu4e-main-mode . org-msg-mode)
  :config
(setq org-msg-options "html-preamble:nil html-postamble:nil toc:nil author:nil email:nil")
(setq org-msg-greeting-fmt "\nHi%s,\n\n")
(setq org-msg-recipient-names `(,user-mail-address . ,user-full-name))
(setq org-msg-greeting-name-limit 3)
(setq org-msg-default-alternatives '((new		. (text html))
                                     (reply-to-html	. (text html))
                                     (reply-to-text	. (text))))
(setq org-msg-convert-citation t)

(setq org-msg-signature (concat "Best Regards,\n\n#+begin_signature\n*"
                                user-full-name
                                "*\n\n" (format-time-string "%Y-%m-%d")
                                "\n#+end_signature")))

;; Get reading list from books directory for org-clock report.
;; The org-clock report scope can be a function.
(defun my/reading-list ()
  "Get reading list."
  (let (reading-list)
    (append reading-list
            (file-expand-wildcards (expand-file-name "denote/books/*.org" my-galaxy)))))

(use-package achive
  :commands achive
  :config
  (setq achive-language 'zh)
  (setq achive-cache-path (expand-file-name "chche/.achive" user-emacs-directory)))

(my/space-leader-def
  "ms" '(achive :wk "Share"))

(add-to-list 'display-buffer-alist '("\\*A Chive\\*"
                                     (display-buffer-in-new-tab)
                                     (tab-name . "Share")
                                     (tab-group . "Misc")))
(with-eval-after-load 'evil-collection
  (evil-collection-define-key 'normal 'achive-visual-mode-map
    "q" 'quit-window))

(defun mac-launchpad/string-ends-with (s ending)
  "Return non-nil if string S ends with ENDING."
  (cond ((>= (length s) (length ending))
         (let ((elength (length ending)))
           (string= (substring s (- 0 elength)) ending)))
        (t nil)))

(defun mac-launchpad/find-mac-apps (folder)
  (let* ((files (directory-files folder))
         (without-dots (cl-delete-if (lambda (f) (or (string= "." f) (string= ".." f))) files))
         (all-files (mapcar (lambda (f) (file-name-as-directory (concat (file-name-as-directory folder) f))) without-dots))
         (result (cl-delete-if-not (lambda (s) (mac-launchpad/string-ends-with s ".app/")) all-files)))
    result))

(defun mac-launchpad ()
  (interactive)
  (let* ((apps (mac-launchpad/find-mac-apps "/Applications"))
         (to-launch (completing-read "launch: " apps)))
    (shell-command (format "defaults read \"%s\"Contents/Info.plist CFBundleIdentifier | xargs open -b" to-launch))))

(my/space-leader-def
  "mj" '(mac-launchpad :wk "Jump to App"))

(defun xah-show-in-desktop ()
  "Show current file in desktop.
This command can be called when in a file buffer or in `dired'."
  (interactive)
  (let (($path (if (buffer-file-name) (buffer-file-name) default-directory)))
    (cond
     ((string-equal system-type "windows-nt")
      (shell-command
       (format "PowerShell -Command Start-Process Explorer -FilePath %s"
               (shell-quote-argument default-directory))))
     ((string-equal system-type "darwin")
      (if (eq major-mode 'dired-mode)
          (let (($files (dired-get-marked-files )))
            (if (eq (length $files) 0)
                (shell-command (concat "open " (shell-quote-argument (expand-file-name default-directory ))))
              (shell-command (concat "open -R " (shell-quote-argument (car (dired-get-marked-files )))))))
        (shell-command
         (concat "open -R " (shell-quote-argument $path)))))
     ((string-equal system-type "gnu/linux")
      (let ((process-connection-type nil)
            (openFileProgram (if (file-exists-p "/usr/bin/gvfs-open")
                                 "/usr/bin/gvfs-open"
                               "/usr/bin/xdg-open")))
        (start-process "" nil openFileProgram (shell-quote-argument $path)))))))

(my/space-leader-def
  "of" '(xah-show-in-desktop :wk "Open Finder"))

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

(with-eval-after-load 'evil
  (evil-declare-key 'normal 'global
    "gX" 'jf/org-link-remove-link))

(defun yt-set-time (time)
  "Set TIME in the YouTube link at point.)
  TIME is number of seconds if called from Lisp, and a string if
  called interactively.
  Supported formats:
  - seconds
  - minutes:seconds
  - number of seconds with the \"s\" suffix."
  (interactive (list
                (if current-prefix-arg
                    (prefix-numeric-value current-prefix-arg)
                  (read-string "Time: "))))
  (let ((url (thing-at-point-url-at-point)))
    (if (and url
             (string-match
              (format "^%s"
                      (regexp-opt
                       '("https://www.youtube.com/"
                         "https://youtu.be/")
                       "\\(?:")))
             url))
    (let* ((bounds (thing-at-point-bounds-of-url-at-point))
           (time-present-p (string-match "t=[0-9]+" url))
           (question-mark-present-p (string-search "?" url))
           (seconds (cond
                     ((numberp time)
                      time)
                     ((string-match
                       "^\\([0-9]+\\):\\([0-9]\\{2\\}\\)$" time)
                      (+ (* 60 (string-to-number
                                (match-string 1 time)))
                         (string-to-number (match-string 2 time))))
                     ((string-match "^\\([0-9]+\\)s?$" time)
                      (string-to-number (match-string 1 time)))
                     (t (error "Wrong argument format"))))
           (new-url (if time-present-p
                        (replace-regexp-in-string
                         "t=[0-9]+"
                         (format "t=%i" seconds)
                         url)
                      (concat url
                              (if question-mark-present-p "&" "?")
                              (format "t=%i" seconds)))))
      (delete-region (car bounds) (cdr bounds))
      (insert new-url))
    (error "Not on a Youtube link")))

(my/space-leader-def
  "my" '(yt-set-time :wk "Youtube link time"))

(defun switch-to-message ()
  "Quick switch to `*Message*' buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun switch-to-scratch ()
  "Quick switch to `*Scratch*' buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(my/space-leader-def
  "bs" '(switch-to-scratch :wk "*scratch*")
  "bm" '(switch-to-message :wk "*message*"))

(defun my/inbox-file ()
  "Open inbox file."
  (interactive)
  (find-file (expand-file-name "inbox/inbox.org" my-galaxy)))

(defun my/plan-file ()
  "Open plan file."
  (interactive)
  (find-file (expand-file-name "inbox/plan.org" my-galaxy)))

(defun my/index-file ()
  (interactive)
  (find-file (expand-file-name "roam/main/index.org" my-galaxy)))

(defun my/reflection-file ()
  (interactive)
  (find-file (expand-file-name "roam/main/reflection.org" my-galaxy)))

(defun my/finance-file ()
  "Open finance file."
  (interactive)
  (find-file (expand-file-name "finance/beans/finance.bean" my-galaxy)))

(defun my/reading-record ()
  "Open reading record file."
  (interactive)
  (find-file (expand-file-name "denote/books/20230301T211439--Book-lists-and-reading-record__reading.org" my-galaxy)))

(my/space-leader-def
  "f" '(:ignore t :wk "Open files")
  "fb" '(my/reading-record :wk "Reading record")
  "fi" '(my/inbox-file :wk "Inbox file")
  "fI" '(my/index-file :wk "Index file")
  "fp" '(my/plan-file :wk "Plan file")
  "ff" '(my/finance-file :wk "Finance file")
  "fR" '(my/reflection-file :wk "Reflection file")
  "fg" '(my/gtd-file :wk "GTD file"))

(defun my/start-server ()
  (interactive)
  (if (not (server-running-p))
      (server-start))
  (message "Server has started"))

(defun my/scroll-other-windown-down ()
  "Scroll other window down."
  (interactive)
  (scroll-other-window-down 2))

(global-set-key (kbd "M-p") 'my/scroll-other-windown-down)

(defun my/scroll-other-windown ()
  "Scroll other window up."
  (interactive)
  (scroll-other-window 2))

(global-set-key (kbd "M-n") 'my/scroll-other-windown)

;;; init.el ends here.
