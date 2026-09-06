;; -*- lexical-binding: t; -*-

(when (display-graphic-p)
  (when sys/win32p
    (set-face-attribute 'default nil :family "Iosevka" :height 120)
    (dolist (charset '(kana han cjk-misc bopomofo symbol))
      (set-fontset-font (frame-parameter nil 'font) charset
			(font-spec :family "LXGW WenKai Mono"))))

  (when sys/macp
    (set-face-attribute 'default nil :family "Maple Mono CN" :height 140))

  (set-fontset-font t 'unicode (font-spec :family "Symbols Nerd Font Mono" :size 12) nil 'prepend))


;; (setq face-font-rescale-alist
;;       '(("Source Han Serif SC" . 1.2)))

;; (setq-default line-spacing 0.10)

(provide 'init-font)
