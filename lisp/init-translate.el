(use-package langtool
  :commands langtool-check-buffer
  :config
  (setq langtool-http-server-host "localhost")
  (setq langtool-http-server-port 8081)
  (setq langtool-autoshow-message-function #'langtool-popup-autoshow))

(use-package dictionary
  :bind ("M-#" . dictionary-lookup-definition)
  :config
  (add-to-list 'display-buffer-alist '("^\\*Dictionary\\*"
                                         (display-buffer-in-side-window)
                                         (side . right)
                                         (window-width . 70))))

(use-package go-translate
  :commands gts-do-translate
  :config
  (setq gts-buffer-follow-p t)
  (setq gts-translate-list '(("en" "zh")))
  (setq gts-default-translator (gts-translator
                                :picker (gts-noprompt-picker)
                                :engines (list
                                          (gts-bing-engine)
                                          (gts-google-engine :parser (gts-google-summary-parser)))
                                :render (gts-buffer-render)))
  (defun go-translate-save-kill-ring ()
    (interactive)
    (gts-translate (gts-translator
                    :picker (gts-noprompt-picker)
                    :engines (gts-google-engine
                              :parser (gts-google-summary-parser))
                    :render (gts-kill-ring-render)))))

(with-eval-after-load 'evil
  (evil-define-key '(normal visual) 'global
    "gll" 'gts-do-translate
    "glg" 'go-translate-save-kill-ring))

(use-package lingva
  :commands lingva-translate
  :config
  (setq lingva-target "zh"))

(with-eval-after-load 'evil
  (evil-define-key '(normal visual) 'global
    "glL" 'lingva-translate))

(use-package sdcv
  :commands sdcv-search-pointer sdcv-search-pointer+
  :config
  (face-spec-set 'sdcv-tooltip-face
                 '((((background light))
                    :foreground "#000000" :background "#ffffff")
                   (t
                    :foreground "#ffffff" :background "#000000"))
                 'face-override-spec)

  (setq sdcv-tooltip-border-width 2)
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

(with-eval-after-load 'evil-collection
    (evil-define-key '(normal visual) 'global
      "glP" 'sdcv-search-pointer
      "glp" 'sdcv-search-pointer+)

  (evil-collection-define-key 'normal 'sdcv-mode-map
    "q" 'quit-window))

(use-package osx-dictionary
  :commands osx-dictionary-search-pointer)

(with-eval-after-load 'evil
  (evil-define-key '(normal visual) 'global
    "gld" 'osx-dictionary-search-pointer))

(use-package powerthesaurus
  :commands (powerthesaurus-lookup-dwim
             powerthesaurus-lookup-related-dwim
             powerthesaurus-lookup-synonyms-dwim
             powerthesaurus-lookup-antonyms-dwim
             powerthesaurus-lookup-definitions-dwim
             powerthesaurus-lookup-sentences-dwim))

(with-eval-after-load 'evil
  (evil-define-key '(normal visual) 'global
    "glt" 'powerthesaurus-lookup-dwim))

(use-package dictionary-overlay
  :commands dictionary-overlay-render-buffer dictionary-overlay-toggle
  :config
  (setq dictionary-overlay-translators '("local" "darwin" "sdcv" "web"))
  (setq dictionary-overlay-user-data-directory
        (expand-file-name "cache/dictionary-overlay" user-emacs-directory))
  (setq dictionary-overlay-python "/opt/homebrew/bin/python3.10")
  (dictionary-overlay-start))

(with-eval-after-load 'evil
  (evil-define-key '(normal visual) 'global
    "glr" 'dictionary-overlay-toggle
    "glk" 'dictionary-overlay-mark-word-unknown
    "glK" 'dictionary-overlay-mark-word-known))

(defun my/siri-translate ()
  (interactive)
  (let ((tempfile (make-temp-file "siri-translate-" nil ".txt")))
    (write-region
     (format "%s" (thing-at-point 'paragraph)) nil tempfile)
    (end-of-paragraph-text)
    (shell-command (format "shortcuts run \"Translate File\" -i %s &" tempfile)))
  (shell-command "open -b org.gnu.Emacs"))

(defun my/siri-translate2english ()
  (interactive)
  (let ((tempfile (make-temp-file "siri-translate-" nil ".txt")))
    (write-region
     (format "%s" (thing-at-point 'paragraph)) nil tempfile)
    (end-of-paragraph-text)
    (shell-command (format "shortcuts run \"Translate File 2 English\" -i %s &" tempfile)))
  (shell-command "open -b org.gnu.Emacs"))

(defun language-to-zh-or-zh-to-english ()
  (interactive)
  (let ((string (thing-at-point 'paragraph)))
    (if (eq (string-match "\\cC" string) nil)
        (my/siri-translate)
      (my/siri-translate2english))))

(use-package emacs-azure-tts
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
                            (list (expand-file-name file data-directory))))))
  (defun emacs-azure-tts-sentence ()
    (interactive)
    (emacs-azure-tts 1)))

(with-eval-after-load 'evil
  (evil-define-key '(normal visual) 'global
    "gls" 'emacs-azure-tts-sentence))

(use-package language-chunk
  :commands lc-memo-review lc-corpus-capture-card
  :config
  (setq lc-db-location (expand-file-name "database/lc.db" my-galaxy)))

(with-eval-after-load 'evil
  (evil-define-key '(normal visual) 'global
    "glc" 'lc-corpus-capture-card
    "glv" 'lc-memo-review))

(use-package popweb-dict
  :commands popweb-dict-say-word
  :config
  (setq popweb-config-location (expand-file-name "cache/popweb" user-emacs-directory)))

(with-eval-after-load 'evil
  (evil-define-key '(normal visual) 'global
    "glw" 'popweb-dict-say-word))

(provide 'init-translate)
;;; init-translate.el ends here.
