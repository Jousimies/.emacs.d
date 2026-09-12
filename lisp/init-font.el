;; -*- lexical-binding: t; -*-

(defgroup my-fonts nil
  "Cross-platform fonts used by this configuration."
  :group 'faces)

(defcustom my/default-font-candidates
  (if sys/win32p
      '("Iosevka" "Consolas" "Courier New")
    '("Maple Mono CN" "Iosevka" "Menlo" "Monaco"))
  "Preferred default font families, in fallback order."
  :type '(repeat string)
  :group 'my-fonts)

(defcustom my/default-font-height (if sys/win32p 120 140)
  "Default graphical frame font height in tenths of a point."
  :type 'integer
  :group 'my-fonts)

(defcustom my/cjk-font-candidates
  '("LXGW WenKai Mono" "Maple Mono CN" "PingFang SC" "Microsoft YaHei")
  "Preferred CJK font families, in fallback order."
  :type '(repeat string)
  :group 'my-fonts)

(defcustom my/symbol-font-candidates
  '("Symbols Nerd Font Mono" "Apple Symbols" "Segoe UI Symbol")
  "Preferred symbol font families, in fallback order."
  :type '(repeat string)
  :group 'my-fonts)

(defun my/first-available-font (families &optional frame)
  "Return the first installed font in FAMILIES for FRAME."
  (seq-find (lambda (family)
              (find-font (font-spec :family family) frame))
            families))

(defun my/apply-frame-font (&optional frame)
  "Apply configured fonts to graphical FRAME with safe fallbacks."
  (let ((frame (or frame (selected-frame))))
    (when (display-graphic-p frame)
      (with-selected-frame frame
        (when-let* ((family (my/first-available-font
                             my/default-font-candidates frame)))
          (set-face-attribute 'default frame
                              :family family
                              :height my/default-font-height))
        (when-let* ((family (my/first-available-font
                             my/cjk-font-candidates frame)))
          (dolist (charset '(kana han cjk-misc bopomofo))
            (set-fontset-font (frame-parameter frame 'font) charset
                              (font-spec :family family) frame 'prepend)))
        (when-let* ((family (my/first-available-font
                             my/symbol-font-candidates frame)))
          (set-fontset-font (frame-parameter frame 'font) 'symbol
                            (font-spec :family family) frame 'prepend))))))

(when (display-graphic-p)
  (my/apply-frame-font))
(add-hook 'server-after-make-frame-hook #'my/apply-frame-font)


;; (setq face-font-rescale-alist
;;       '(("Source Han Serif SC" . 1.2)))

;; (setq-default line-spacing 0.10)

(provide 'init-font)
