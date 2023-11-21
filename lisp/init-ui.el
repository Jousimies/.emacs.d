;; init-ui.el --- User interface. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(defun toggle-appearance ()
  (interactive)
  (shell-command-to-string "osascript -e 'tell app \"System Events\" to tell appearance preferences to set dark mode to not dark mode'"))

(global-set-key (kbd "C-c t a") 'toggle-appearance)

(set-face-attribute 'default nil :font "Iosevka Term" :weight 'normal :height 140)

;; Source Han Serif SC -> TsangerJinKai02
(set-fontset-font t 'unicode (font-spec :family "Apple Color Emoj" :size 11.5) nil 'prepend)

(set-fontset-font t 'unicode (font-spec :family "Hack Nerd Font Mono" :size 14) nil 'prepend)

(dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset (font-spec :family "TsangerJinKai02" :height 140))
    t 'prepend)

(use-package nerd-icons
  :load-path "packages/nerd-icons.el/"
  :commands nerd-icons-codicon nerd-icons-faicon nerd-icons-icon-for-file
  :config
  (setq nerd-icons-font-family "Hack Nerd Font Mono"))

(use-package nerd-icons-completion
  :load-path "~/.emacs.d/packages/nerd-icons-completion/"
  :hook (minibuffer-setup . nerd-icons-completion-mode))

(use-package nerd-icons-dired
  :load-path "packages/emacs-nerd-icons-dired"
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :load-path "~/.emacs.d/packages/nerd-icons-ibuffer/"
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package tab-bar
  :hook (after-init . tab-bar-mode)
  :bind (("C-c b t" . tab-switch)
         ("s-t" . tab-new)
         ("s-w" . tab-close))
  :hook (after-make-frame-functions . toggle-frame-tab-bar)
  :config
  (setq tab-bar-new-tab-choice 'scratch-buffer)
  (setq tab-bar-format '(tab-bar-format-history
                         tab-bar-format-tabs
                         tab-bar-separator
                         tab-bar-format-align-right))
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-separator "​​")
  (setq tab-bar-select-tab-modifiers '(super))
  (setq tab-bar-tab-hints t))

(defun tab-bar-format-menu-bar ()
  "Produce the Menu button for the tab bar that shows the menu bar."
  `((menu-bar menu-item (propertize "  " 'face nil)
              tab-bar-menu-bar :help "Menu Bar")))

(defun my/tab-bar-tab-name-format-comfortable (tab i)
  (propertize (concat " " (tab-bar-tab-name-format-default tab i) " ")
              'face (funcall tab-bar-tab-face-function tab)))

(setq tab-bar-tab-name-format-function #'my/tab-bar-tab-name-format-comfortable)

;; https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/prot-lisp/prot-modeline.el
(defcustom prot-modeline-string-truncate-length 9
  "String length after which truncation should be done in small windows."
  :type 'natnum)

(defun prot-modeline--string-truncate-p (str)
  "Return non-nil if STR should be truncated."
  (and (< (window-total-width) split-width-threshold)
       (> (length str) prot-modeline-string-truncate-length)))

(defun prot-modeline-string-truncate (str)
  "Return truncated STR, if appropriate, else return STR.
  Truncation is done up to `prot-modeline-string-truncate-length'."
  (if (prot-modeline--string-truncate-p str)
      (concat (substring str 0 prot-modeline-string-truncate-length) "...")
    str))

(defun my/modeline--major-mode ()
  (capitalize (string-replace "-mode" "" (symbol-name major-mode))))

(defvar-local my/modeline-major-mode
    '(:eval (when (mode-line-window-selected-p)
              (propertize (my/modeline--major-mode) 'face `(:inherit font-lock-variable-name-face)))))

(defun my/modeline--buffer-name ()
  (when-let ((name (buffer-name)))
    (prot-modeline-string-truncate name)))

(defun my/modeline-buffer-name ()
  (let ((name (my/modeline--buffer-name)))
    (format "%s" name)))

(defun my/modeline--file-name ()
  (when-let ((name (buffer-file-name)))
    (prot-modeline-string-truncate (file-name-nondirectory name))))

(defun my/modeline-file-name ()
  (let ((name (my/modeline--file-name)))
    (if name
        (format "%s" name)
      (my/modeline-buffer-name))))

(defvar-local my/modeline-file-name
    '(:eval (when (mode-line-window-selected-p)
              (propertize (my/modeline-file-name) 'face 'bold))))

(defvar-local my/modeline-buffer-readonly
    '(:eval (when buffer-read-only
              (propertize " "
                          'face nil))))

(defvar-local my/modeline-buffer-modified
    '(:eval (when (mode-line-window-selected-p)
              (propertize " * " 'face `(:inherit ,(if (buffer-modified-p) 'error nil))))))

(defvar-local my/modeline-input-method
    '(:eval (when (mode-line-window-selected-p)
              (propertize
               (if cur-sys-input-method
                   " ZH "
                 " EN ")
               'face `(:inherit ,(if cur-sys-input-method 'font-lock-string-face nil) :inverse-video t)))))

(defvar-local my/modeline-kbd-macro
    '(:eval
      (when (and (mode-line-window-selected-p) defining-kbd-macro)
        (propertize " KMacro " 'face `(:inherit font-lock-constant-face :inverse-video t)))))

(defvar-local my/modeline-region-indicator
    '(:eval (when (and (mode-line-window-selected-p) (use-region-p))
              (propertize
               (concat "| L" (number-to-string (count-lines (region-beginning) (region-end)))
                       " W" (number-to-string (count-words (region-beginning) (region-end)))
                       " C" (number-to-string (abs (- (mark t) (point)))) " ")))))

(defun my/modeline--image-info ()
    (car (process-lines  "identify"  "-format"  "[%m %wx%h %b]" (buffer-file-name))))

(defvar-local my/modeline-image-info
    '(:eval (when (and (mode-line-window-selected-p) (or (eq major-mode 'image-mode)
                                                         (eq major-mode 'telega-image-mode)))
              (propertize (my/modeline--image-info) 'face font-lock-string-face))))

;; https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/prot-lisp/prot-modeline.el
(defun my/modeline--right-align-rest ()
  (format-mode-line
   `(""
     ,@(cdr (memq 'my/modeline-align-right mode-line-format)))))

(defun my/modeline--right-align-width ()
  (string-pixel-width (my/modeline--right-align-rest)))

(defun my/modeline--box-p ()
  "Return non-nil if the `mode-line' has a box attribute."
  (and (face-attribute 'mode-line :box)
       (null (eq (face-attribute 'mode-line :box) 'unspecified))))

(defun my/modeline--magic-number ()
  (let ((height (face-attribute 'mode-line :height nil 'default))
        (m-width (string-pixel-width (propertize "m" 'face 'mode-line))))
    (round height (* m-width (* height m-width 0.001)))))

(defvar-local my/modeline-align-right
    '(:eval
      (propertize
       " "
       'display
       `(space
           :align-to
           (- right
              right-fringe
              right-margin
              ,(ceiling
                (my/modeline--right-align-width)
                (string-pixel-width (propertize "m" 'face 'mode-line))))))))

(defvar-local my/modeline-date
    '(:eval (when (and (mode-line-window-selected-p) (> (window-width) 90))
              (propertize (format-time-string " %Y-%m-%d %a ") 'face `(:inherit success)))))

(defvar org-timer-countdown-timer nil)
(defun my/modeline--timer ()
  (when org-timer-countdown-timer
        (concat " " (org-timer-value-string))))

(defvar-local my/modeline-timer
    '(:eval (when (and (mode-line-window-selected-p) org-timer-countdown-timer)
              (propertize (my/modeline--timer) 'face `(:inherit error :inverse-video t)))))

(defvar-local my/modeline-time
    '(:eval (when (mode-line-window-selected-p)
              (propertize (format-time-string " %H:%MPM ") 'face `(:inherit success :inverse-video t)))))

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
    '(:eval (when (mode-line-window-selected-p)
              (propertize (concat (my/modeline--sys-coding-category) (my/modeline--sys-coding-eol)) 'face nil))))

(defun my/modeline--pdf-page ()
  (format " %d/%d " (eval '(pdf-view-current-page)) (pdf-cache-number-of-pages)))

(defvar-local my/modeline-position
    '(:eval (if (and (mode-line-window-selected-p) (derived-mode-p 'pdf-view-mode))
                (propertize (my/modeline--pdf-page) 'face font-lock-string-face)
              (propertize (format " %%l:%%c/%d " (line-number-at-pos (point-max))) 'face nil))))

(defvar-local my/modeline-clock-info
    '(:eval (when (and (mode-line-window-selected-p) (org-clocking-p))
              (propertize (format " [%s](%s)"
                                  (org-duration-from-minutes
                                   (floor (org-time-convert-to-integer
                                           (org-time-since org-clock-start-time))
                                          60))
                                  org-clock-heading)
                          'face `(:inherit font-lock-builtin-face)))))

(defun my/modeline--battery-data ()
  (and battery-status-function
                            (functionp battery-status-function)
                            (funcall battery-status-function)))

(defun my/modeline--battery-status ()
  (cdr (assoc ?L (my/modeline--battery-data))))

(defun my/modeline--battery-percentage ()
  (car (read-from-string (or (cdr (assq ?p (my/modeline--battery-data))) "ERR"))))

(defun my/modeline--battery ()
  (let* ((charging? (string-equal "AC" (my/modeline--battery-status)))
         (percentage (my/modeline--battery-percentage)))
    (if charging?
        (format "󱐋%d%s" percentage "%%")
      (cond ((>= percentage 80) (format "󰁹%d%s" percentage "%%"))
            ((>= percentage 70) (format "󰂀%d%s" percentage "%%"))
            ((>= percentage 60) (format "󰁿%d%s" percentage "%%"))
            ((>= percentage 50) (format "󰁾%d%s" percentage "%%"))
            ((>= percentage 40) (format "󰁽%d%s" percentage "%%"))
            ((>= percentage 30) (format "󰁼%d%s" percentage "%%"))
            ((>= percentage 20) (format "󰁻%d%s" percentage "%%"))
            ((< percentage 20) (format "󰂎%d%s" percentage "%%"))))))

(defvar-local my/modeline-battery
    '(:eval (when (mode-line-window-selected-p)
              (propertize (my/modeline--battery) 'face `(:inherit ,(if (< (my/modeline--battery-percentage) 20)
                                                                     'error nil))))))

(dolist (construct '(my/modeline-major-mode
                     my/modeline-buffer-indentification
                     my/modeline-input-method
                     my/modeline-kbd-macro
                     my/modeline-region-indicator
                     my/modeline-align-right
                     my/modeline-file-name
                     my/modeline-buffer-readonly
                     my/modeline-buffer-modified
                     my/modeline-date
                     my/modeline-time
                     my/modeline-timer
                     my/modeline-sys
                     my/modeline-battery
                     my/modeline-position
                     my/modeline-image-info
                     my/modeline-clock-info))
  (put construct 'risky-local-variable t))

(setq-default mode-line-format
              '("%e"
                my/modeline-input-method
                my/modeline-buffer-readonly
                my/modeline-buffer-modified
                my/modeline-file-name
                my/modeline-position
                my/modeline-image-info
                my/modeline-kbd-macro
                my/modeline-region-indicator
                "       "
                my/modeline-align-right
                my/modeline-major-mode
                my/modeline-sys
                my/modeline-timer
                (:eval (with-eval-after-load 'org-clock
                         my/modeline-clock-info))
                my/modeline-date
                my/modeline-time
                " "
                battery-mode-line-string))

(define-fringe-bitmap 'right-curly-arrow  [])
(define-fringe-bitmap 'left-curly-arrow  [])

(fringe-mode '(1 . 1))

(use-package hl-line
  :hook ((prog-mode . hl-line-mode)
         (org-mode . hl-line-mode)
         (profiler-report-mode . hl-line-mode)))

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  :init
  (setq-default display-line-numbers-widen t)
  :config
  (setq display-line-numbers-type 'relative))

(scroll-bar-mode 0)

(use-package display-fill-column-indicator
  :hook (prog-mode . display-fill-column-indicator-mode)
  :config
  (face-spec-set 'fill-column-indicator
                 '((default :height 0.1))
                 'face-override-spec)
  (setq-default fill-column 90))

(use-package paren
  :hook (text-mode . show-paren-mode)
  :config
  (setq show-paren-style 'parenthesis)
  (setq show-paren-context-when-offscreen 'overlay))

(use-package rainbow-mode
  :load-path "packages/rainbow-mode/"
  :hook (prog-mode . rainbow-mode))

(use-package pulse
  :preface
  (defun my-pulse-momentary (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point) 'next-error))

  (defun my-recenter (&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (my-pulse-momentary))

  :hook ((bookmark-after-jump next-error other-window
                              dumb-jump-after-jump imenu-after-jump) . my-recenter)
  :init (dolist (cmd '(recenter-top-bottom
                       other-window windmove-do-window-select
                       pop-to-mark-command pop-global-mark
                       pager-page-down pager-page-up))
          (advice-add cmd :after #'my-pulse-momentary)))

(use-package form-feed
  :load-path "packages/form-feed/"
  :hook (org-mode . form-feed-mode))

(use-package frame
  :hook (after-init . (lambda ()
                        (blink-cursor-mode -1)
                        (add-to-list 'after-make-frame-functions #'ct/frame-center 0)))
  :custom
  (window-divider-default-bottom-width 1)
  (window-divider-default-places 'bottom-only))

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

(use-package winner
  :hook (after-init . winner-mode)
  :config
  (setq winner-dont-bind-my-keys t)
  (setq winner-boring-buffers '("*Completions*"
                                "*Compile-Log*"
                                "*inferior-lisp*"
                                "*Fuzzy Completions*"
                                "*Apropos*"
                                "*Help*"
                                "*cvs*"
                                "*Buffer List*"
                                "*Ibuffer*"
                                "*esh command on file*")))

(use-package window
  :init
  (add-to-list 'display-buffer-alist '("\\*Outline"
                                       (display-buffer-in-side-window)
                                       (side . right)
                                       (window-width . 0.5)))
  :custom
  (switch-to-buffer-in-dedicated-window 'pop)
  (switch-to-buffer-obey-display-actions t))

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

(use-package ace-window
  :load-path "packages/ace-window"
  :bind ("M-o" . ace-window))

(use-package windmove
  :hook (after-init . windmove-mode)
  :bind (("C-c <up>" . windmove-up)
         ("C-c <down>" . windmove-down)))

(use-package bufferlo
  :load-path "packages/bufferlo/"
  :bind (([remap switch-to-buffer] . bufferlo-switch-to-buffer))
  :hook (after-init . bufferlo-mode))

(use-package perspective
  :load-path "packages/perspective-el/"
  :bind (("M-s-n" . persp-switch)
         ("M-s-w" . persp-kill))
  :custom
  (persp-mode-prefix-key (kbd "C-c z"))
  :hook ((after-init . persp-mode)
         (kill-emacs . persp-state-save))
  :config
  (with-eval-after-load 'tab-bar

    (defun tab-bar-format-persp ()
      (setq global-mode-string (delete '(:eval (persp-mode-line)) global-mode-string))
      `((global menu-item ,(format-mode-line (persp-mode-line)) ignore)))
    (add-to-list 'tab-bar-format 'tab-bar-format-persp)))

(with-eval-after-load 'tab-bar
  (with-eval-after-load 'perspective
    (add-to-list 'tab-bar-format 'tab-bar-format-menu-bar)))

(use-package popper
  :load-path "packages/popper/"
  :bind (:map popper-mode-map
              ("M-<tab>"   . popper-cycle)
              ("M-`" . popper-toggle-type))
  :hook (emacs-startup . popper-mode)
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
          "\\*tldr\\*$"
          "\\*vc-.*\\*$"
          "^\\*elfeed-entry\\*$"
          "^\\*macro expansion\\**"

          "\\*TeX Help\\*"
          "^\\*denote-backlinks to "
          "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Org Note\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"))
  :config
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

(use-package popper-echo
  :hook (popper-mode . popper-echo-mode))

(use-package surround
  :load-path "packages/surround/"
  :bind-keymap ("M-'" . surround-keymap))

(provide 'init-ui)
;;; init-ui.el ends here.
