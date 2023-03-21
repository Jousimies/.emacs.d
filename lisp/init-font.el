(set-face-attribute 'default nil :font "Iosevka Term" :height 160)
(set-frame-font "Iosevka Term 16" nil t)
(when (display-graphic-p)
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset (font-spec :family "Source Han Serif SC" :height 140)) t 'prepend))

(set-fontset-font t 'unicode (font-spec :family "Hack Nerd Font Mono") nil 'prepend)

(provide 'init-font)
;;; init-font.el ends here.
