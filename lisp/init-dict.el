;; init-dict.el --- English and other language learning. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

;; init-dict.el --- Dict, spell and grammar check. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

;; The buitin dictonary, works not well.
;; (use-package dictionary
;;   :config
;;   (add-to-list 'display-buffer-alist '("^\\*Dictionary\\*"
;;                                        (display-buffer-in-side-window)
;;                                        (side . right)
;;                                        (window-width . 70))))

;; The result not display well.
(use-package osx-dictionary)

(use-package sdcv
  :load-path "packages/sdcv/"
  :commands sdcv-search-pointer sdcv-search-pointer+ sdcv-search-input sdcv-search-input+
  ;; :hook (after-init . my/sdcv-tooltip-face-toggle)
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

(use-package powerthesaurus)

(use-package go-translate
  :custom
  (gt-buffer-render-follow-p t)
  (gt-langs '("en" "zh"))
  (gt-default-translator
   (gt-translator :engines (gt-google-engine :cache 'word)
				  :render (gt-buffer-render :then (gt-kill-ring-render)))))

(defun my/translate ()
  (interactive)
  (if (use-region-p)
      (gt-do-translate)
    (sdcv-search-pointer+)))

;; (use-package dictionary-overlay
;;   :load-path "packages/dictionary-overlay/" "packages/websocket-bridge/" "packages/emacs-websocket/"
;;   :commands dictionary-overlay-toggle dictionary-overlay-render-buffer dictionary-overlay-mark-word-unknown dictionary-overlay-mark-word-known
;;   :config
;;   (setq dictionary-overlay-translators '("local" "darwin" "sdcv" "web"))
;;   (setq dictionary-overlay-user-data-directory
;;         (expand-file-name "dictionary-overlay" cache-directory))
;;   (setq dictionary-overlay-python "/opt/homebrew/bin/python3.10")
;;   (dictionary-overlay-start))

;; Emacs 内置的 ispell 和 flyspell 没有 jinx 性能好
(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind ("M-#" . jinx-correct)
  :config
  (add-to-list 'jinx-exclude-regexps '(t "\\cc")))

(use-package writegood-mode
  :custom
  (writegood-weasel-words
   '("very" "rather" "really" "quite" "in fact" "just" "so" "pretty" "of course" "surely" "that said" "actually")))


(provide 'init-dict)
;;; init-dict.el ends here.
