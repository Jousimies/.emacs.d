;; init-dict.el --- English and other language learning. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

;; init-dict.el --- Dict, spell and grammar check. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package dictionary
  :commands dictionary-lookup-definition
  :config
  (add-to-list 'display-buffer-alist '("^\\*Dictionary\\*"
                                         (display-buffer-in-side-window)
                                         (side . right)
                                         (window-width . 70))))

(use-package osx-dictionary
  :load-path "packages/osx-dictionary.el/"
  :commands osx-dictionary-search-pointer)

(use-package sdcv
  :load-path "packages/sdcv/"
  :bind (("M-#" . my/search-dictionary)
         ("C-c l i" . sdcv-search-input))
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

(use-package powerthesaurus
  :load-path ("packages/emacs-powerthesaurus/" "packages/jeison")
  :bind ("C-c l p" . powerthesaurus-lookup-dwim))

(use-package popweb-dict
  :load-path ("packages/popweb/" "packages/popweb/extension/dict")
  :bind ("C-c l s" . popweb-dict-say-word))

(use-package emacs-azure-tts
  :load-path "packages/emacs-azure-tts/"
  :commands emacs-azure-tts
  :preface
  (defun play-sound-internal (sound)
  "Internal function for `play-sound' (which see)."
  (or (eq (car-safe sound) 'sound)
      (signal 'wrong-type-argument (list sound)))

  (cl-destructuring-bind (&key file data volume device)
      (cdr sound)

    (and (or data device)
         (error "DATA and DEVICE arg not supported"))

    (apply #'start-process "afplay" nil
           "afplay" (append (and volume (list "-v" volume))
                            (list (expand-file-name file data-directory)))))))

(defun emacs-azure-tts-sentence ()
    (interactive)
    (emacs-azure-tts 1))
(global-set-key (kbd "C-c l S") 'emacs-azure-tts-sentence)

(use-package go-translate
  :load-path "packages/go-translate/"
  :bind (("C-c l l" . my/gts-do-translate))
  :commands gts-translate gts-do-translate
  :config
  (add-to-list 'display-buffer-alist '("^\\*Go-Translate\\*"
                                           (display-buffer-in-side-window)
                                           (side . bottom)
                                           (height . 0.3)))
  (setq gts-buffer-follow-p t)
  (setq gts-translate-list '(("en" "zh")))
  (setq gts-default-translator (gts-translator
                                :picker (gts-noprompt-picker)
                                :engines (list
                                          ;; (gts-google-rpc-engine)
                                          (gts-google-engine :parser (gts-google-summary-parser))
                                          )
                                :render (gts-buffer-render))))
;;;###autoload
(defun my/gts-do-translate (arg)
  "Prompt for input and perform translation, displaying output in split window.
 With prefix argument, instead save translation to kill-ring."
  (interactive "P")
  (if arg
      (gts-translate (gts-translator
                      :picker (gts-noprompt-picker)
                      :engines (gts-google-engine
                                :parser (gts-google-summary-parser))
                      :render (gts-kill-ring-render)))
    (gts-do-translate)))

(use-package dictionary-overlay
  :load-path "packages/dictionary-overlay/" "packages/websocket-bridge/" "packages/emacs-websocket/"
  :bind (("C-c l r" . dictionary-overlay-toggle)
         ("C-c l R" . dictionary-overlay-render-buffer))
  :config
  (setq dictionary-overlay-translators '("local" "darwin" "sdcv" "web"))
  (setq dictionary-overlay-user-data-directory
        (expand-file-name "cache/dictionary-overlay" user-emacs-directory))
  (setq dictionary-overlay-python "/opt/homebrew/bin/python3.10")
  (dictionary-overlay-start))

(use-package jinx
  :load-path "packages/jinx/"
  :hook (text-mode . jinx-mode)
  :bind ("s-;" . jinx-correct)
  :config
  (add-to-list 'jinx-exclude-regexps '(t "\\cc")))

(use-package writegood-mode
  :load-path "packages/writegood-mode/"
  :bind ("C-c l w" . writegood-mode)
  :config
  (setq writegood-weasel-words
        '("very" "rather" "really" "quite" "in fact" "just" "so" "pretty" "of course" "surely" "that said" "actually")))

(provide 'init-dict)
;;; init-dict.el ends here.
