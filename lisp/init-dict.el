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
  :commands osx-dictionary-search-pointer osx-dictionary-search-input)

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

(use-package powerthesaurus
  :load-path ("packages/emacs-powerthesaurus/" "packages/jeison")
  :commands (powerthesaurus-lookup-synonyms-dwim
			 powerthesaurus-lookup-antonyms-dwim
			 powerthesaurus-lookup-related-dwim
			 powerthesaurus-lookup-definitions-dwim
			 powerthesaurus-lookup-sentences-dwim))

;; (use-package popweb-dict
;;   :load-path ("packages/popweb/" "packages/popweb/extension/dict")
;;   :commands popweb-dict-say-word
;;   :config
;;   (setq popweb-config-location (expand-file-name "popweb" cache-directory)))

;; (use-package emacs-azure-tts
;;   :load-path "packages/emacs-azure-tts/"
;;   :commands emacs-azure-tts
;;   :preface
;;   (defun play-sound-internal (sound)
;;   "Internal function for `play-sound' (which see)."
;;   (or (eq (car-safe sound) 'sound)
;;       (signal 'wrong-type-argument (list sound)))

;;   (cl-destructuring-bind (&key file data volume device)
;;       (cdr sound)

;;     (and (or data device)
;;          (error "DATA and DEVICE arg not supported"))

;;     (apply #'start-process "afplay" nil
;;            "afplay" (append (and volume (list "-v" volume))
;;                             (list (expand-file-name file data-directory)))))))

;; (defun emacs-azure-tts-sentence ()
;;     (interactive)
;;     (emacs-azure-tts 1))

(use-package go-translate
  :load-path "packages/go-translate/"
  :commands gt-translate gt-do-translate gt-do-speak
  :config
  (add-to-list 'display-buffer-alist '("^\\*gt-result\\*"
                                       (display-buffer-in-side-window)
                                       (side . bottom)
                                       (height . 0.3)))
  (setq gt-buffer-render-follow-p t)
  (setq gt-langs '("en" "zh"))
  (setq gt-buffer-render-window-config
          '((display-buffer-reuse-window display-buffer-in-direction)
            (direction . bottom)
            (window-height . 0.4)))
  (setq gt-pop-posframe-forecolor (face-foreground 'tooltip nil t)
        gt-pop-posframe-backcolor (face-background 'tooltip nil t))
  (when (facep 'posframe-border)
      (setq gt-pin-posframe-bdcolor (face-background 'posframe-border nil t)))

  (setq gt-default-translator (gt-translator :engines (gt-google-engine :cache 'word)
											 :render (list (gt-posframe-pop-render :if 'word :frame-params (list :border-width 0 :border-color "red"))
														   (gt-buffer-render :then (gt-kill-ring-render))))))

(use-package dictionary-overlay
  :load-path "packages/dictionary-overlay/" "packages/websocket-bridge/" "packages/emacs-websocket/"
  :commands dictionary-overlay-toggle dictionary-overlay-render-buffer dictionary-overlay-mark-word-unknown dictionary-overlay-mark-word-known
  :config
  (setq dictionary-overlay-translators '("local" "darwin" "sdcv" "web"))
  (setq dictionary-overlay-user-data-directory
        (expand-file-name "dictionary-overlay" cache-directory))
  (setq dictionary-overlay-python "/opt/homebrew/bin/python3.10")
  (dictionary-overlay-start))

(use-package jinx
  :load-path "packages/jinx/"
  :hook (org-mode . jinx-mode)
  :bind ("M-#" . jinx-correct)
  :config
  (add-to-list 'jinx-exclude-regexps '(t "\\cc")))

(use-package writegood-mode
  :load-path "packages/writegood-mode/"
  :commands writegood-mode
  :config
  (setq writegood-weasel-words
        '("very" "rather" "really" "quite" "in fact" "just" "so" "pretty" "of course" "surely" "that said" "actually")))

;; lsp-bridge-toggle-sdcv-helper use pinyin to search english words,
;; Disable corfu-mode to turn off cape-dabbrev temporarily.
(add-to-list 'load-path "~/.emacs.d/packages/lsp-bridge/")
(autoload 'lsp-bridge-toggle-sdcv-helper "lsp-bridge" "" t)
(defun my/toggle-corfu ()
  "Deactivate input method when sdcv helper enabled."
  (interactive)
  (if acm-enable-search-sdcv-words
      (corfu-mode -1)
    (corfu-mode 1)))

(advice-add 'lsp-bridge-toggle-sdcv-helper :after #'my/toggle-corfu)

(defvar dict-file nil)
(setq dict-file "~/.emacs.d/sdcv-dict/words.txt")

;; We open up the dictionary file read-only, so that
;; dabbrev-completion-all can pull from it
(with-current-buffer (find-file-noselect dict-file)
  (set (make-local-variable 'buffer-read-only) t))

(defun dabbrev-completion-all ()
  "convenience function to do dabbrev-completion using all buffers"
  (interactive)
  (dabbrev-completion 16))

(global-set-key (kbd "C-:") 'dabbrev-completion-all)

(provide 'init-dict)
;;; init-dict.el ends here.
