;; init-ui.el --- UI *- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

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

(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'modus-operandi t))
    ('dark (load-theme 'modus-vivendi t))))
(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

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

(provide 'init-ui)
;;; init-ui.el ends here.
