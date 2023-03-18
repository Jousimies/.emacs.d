(set-face-attribute 'default nil :font "Iosevka Term" :height 160)
(when (display-graphic-p)
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset (font-spec :family "Source Han Serif SC" :height 140)) t 'prepend))

(provide 'init-font)
;;; init-font.el ends here.
