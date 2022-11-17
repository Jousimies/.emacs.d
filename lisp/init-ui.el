;;; init-ui.el --- User interface. -*- lexical-binding: t no-byte-compile: t -*-
;;; Code:
;;; Commentary:
;; Fonts and themes
(set-frame-font "Iosevka Fixed 16" nil t)
(if (display-graphic-p)
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
			charset (font-spec :family "Source Han Serif SC" :height 160))))

;; unicode font
(require-package 'unicode-fonts)
(unicode-fonts-setup)

(require-package 'doom-themes)
(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'doom-nord-light t))
    ('dark (load-theme 'doom-nord t))))
(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

(toggle-frame-fullscreen)

(when (maybe-require-package 'all-the-icons)
  (when (display-graphic-p)
    (require 'all-the-icons))
  (with-eval-after-load 'all-the-icons
    (set-fontset-font "fontset-default" 'unicode (font-spec :family "all-the-icons"))  ;;这里不能用 append，否则不工作。
    (set-fontset-font "fontset-default" 'unicode (font-spec :family "file-icons") nil 'append)
    (set-fontset-font "fontset-default" 'unicode (font-spec :family "Material Icons") nil 'append))

  (when (maybe-require-package 'all-the-icons-completion)
    (add-hook 'after-init-hook 'all-the-icons-completion-mode)
    (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)))

;; Do not show curly at fringe.
(define-fringe-bitmap 'right-curly-arrow  [])
(define-fringe-bitmap 'left-curly-arrow  [])

;; Highline current line.
(add-hook 'after-init-hook 'global-hl-line-mode)

;; Keep line numbers inside a narrow
(setq-default display-line-numbers-widen t)
;; Menu bar
(add-hook 'org-mode-hook 'menu-bar--wrap-long-lines-window-edge)
(add-hook 'text-mode-hook 'menu-bar--display-line-numbers-mode-relative)
(add-hook 'prog-mode-hook 'menu-bar--display-line-numbers-mode-relative)
;; Show fill column indicator.
(setq-default fill-column 90)
;; (global-display-fill-column-indicator-mode t)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
;; Show paren.
(setq show-paren-style 'parenthesis)
(setq show-paren-context-when-offscreen 'overlay)

(add-hook 'text-mode-hook 'show-paren-mode)

;;
(when (maybe-require-package 'rainbow-mode)
  (add-hook 'prog-mode-hook 'rainbow-mode))

;;
(when (maybe-require-package 'rainbow-delimiters)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Cursor.
(blink-cursor-mode -1)

;; Beacon
(when (maybe-require-package 'beacon)
  (setq-default beacon-color "Cyan")
  (setq-default beacon-lighter "")
  (setq-default beacon-size 30)
  (add-hook 'after-init-hook 'beacon-mode))

;; Symbol overlay
(when (maybe-require-package 'symbol-overlay)
  (dolist (hook '(prog-mode-hook html-mode-hook yaml-mode-hook conf-mode-hook))
    (add-hook hook 'symbol-overlay-mode))
  (with-eval-after-load 'symbol-overlay
    (define-key symbol-overlay-mode-map (kbd "M-i") 'symbol-overlay-put)
    (define-key symbol-overlay-mode-map (kbd "M-I") 'symbol-overlay-remove-all)
    (define-key symbol-overlay-mode-map (kbd "M-n") 'symbol-overlay-jump-next)
    (define-key symbol-overlay-mode-map (kbd "M-p") 'symbol-overlay-jump-prev)))


;; Page break lines
;; M-x quoted-insert RET C-l to insert page break line.
(when (maybe-require-package 'page-break-lines)
  (add-hook 'after-init-hook 'global-page-break-lines-mode))

(provide 'init-ui)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
