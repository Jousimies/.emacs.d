;; init-ui.el --- User interface. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(set-face-attribute 'default nil :font "Iosevka Term" :height 160)
(when (display-graphic-p)
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset (font-spec :family "Source Han Serif SC" :height 140)) t 'prepend))

(set-fontset-font t 'unicode (font-spec :family "Hack Nerd Font Mono") nil 'prepend)

(use-package nerd-icons
  :commands nerd-icons-codicon nerd-icons-icon-for-file
  :config
  (setq nerd-icons-font-family "Hack Nerd Font Mono"))

(use-package nerd-icons-completion
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package tab-bar
  :hook (after-init . tab-bar-mode)
  :bind ("C-c b t" . tab-switch)
  :config
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-tab-hints nil)
  (setq tab-bar-show nil))

(use-package tabspaces
  :bind ("C-c b b" . tabspaces-switch-to-buffer)
  :hook (tab-bar . tabspaces-mode)
  :config
  (setq tabspaces-session-file
        (expand-file-name "cache/tabsession.el" user-emacs-directory))
  (setq tabspaces-include-buffers '())
  (setq tabspaces-use-filtered-buffers-as-default t))

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

(use-package page-break-lines
  :hook (org-mode . page-break-lines-mode))

(use-package frame
  :defer t
  :hook (after-init . (lambda ()
                        (blink-cursor-mode -1)))
  :config
  (add-to-list 'initial-frame-alist '(alpha . (90 . 100)))
  (face-spec-set 'window-divider
                 '((((background light))
                    :foreground "#000000")
                   (t
                    :foreground "#FFFFFF"))
                 'face-override-spec)
  (setq window-divider-default-bottom-width 1)
  (setq window-divider-default-places 'bottom-only))

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
  (if (= (frame-width) 100) ;; 80 is the default frame width.
      (delete-frame)
    (make-frame)))

(global-set-key (kbd "s-n") 'my/make-or-delete-frame)

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
  :defer t
  :config
  (setq switch-to-buffer-in-dedicated-window 'pop)
  (setq switch-to-buffer-obey-display-actions t)
  (add-to-list 'display-buffer-alist '("\\*Outline"
                                       (display-buffer-in-side-window)
                                       (side . right)
                                       (window-width . 0.5)
                                       (window-parameters
                                        (mode-line-format . none))))
  (add-to-list 'display-buffer-alist '((derived-mode . prog-mode)
                                       (display-buffer-in-tab)
                                       (tab-name . "Porg") (tab-group . "Prog")
                                       (select . t))))

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
  :bind ("M-o" . ace-window))

(provide 'init-ui)
;;; init-ui.el ends here.
