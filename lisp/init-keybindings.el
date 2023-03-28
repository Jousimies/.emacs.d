(use-package which-key
  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order)
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 10000)
  (setq which-key-idle-secondary-delay 0.05)
  :config
  (which-key-mode))

(my/space-leader-def
  ".e" '(emacs-lisp-macroexpand :wk "Macro expand")
  "m" '(:ignore t :wk "Misc")
  "mc" '(gptel :wk "ChatGPT")
  "md" '(disk-usage :wk "Disk usage")
  "mi" '(file-info-show :wk "File info")
  "mj" '(mac-launchpad :wk "Jump to App")
  "mo" '(my/ocr :wk "OCR")
  "mp" '(toggle-proxy :wk "Proxy")
  "ms" '(achive :wk "Share")
  "my" '(yt-set-time :wk "Youtube link time")
  "b" '(:ignore t :wk "Buffer")
  "bs" '(switch-to-scratch :wk "*scratch*")
  "bm" '(switch-to-message :wk "*message*")
  "c" '(calendar :wk "Calendar")
  "t" '(telega :wk "Telega")
  "s" '(:ignore t :wk "Search")
  "sb" '(engine/search-bookdouban :wk "Book")
  "ss" '(engine/search-google :wk "Google")
  "sg" '(engine/search-github :wk "Github")
  "sw" '(engine/search-wikipedia :wk "Wiki")
  "sm" '(engine/search-moviedouban :wk "Movie")
  "sz" '(engine/search-zhihu :wk "Zhihu")
  "sr" '(rg :wk "rg")
  "sl" '(consult-git-grep :wk "git")
  "v" '(toggle-vterm :wk "vterm")
  "f" 'my/hydra-open-file/body)

(evil-define-key '(normal visual motion) 'global
  "gb" 'tabspaces-switch-to-buffer
  "gs" 'tab-switch
  "gX" 'jf/org-link-remove-link
  "gF" 'embark-open-externally
  "glc" 'lc-corpus-capture-card
  "gld" 'osx-dictionary-search-pointer
  ;; "glh" 'lsp-bridge-toggle-sdcv-helper
  "glk" 'dictionary-overlay-mark-word-unknown
  "glK" 'dictionary-overlay-mark-word-known
  "gll" 'my/gts-do-translate
  "glL" 'lingva-translate
  "glP" 'sdcv-search-pointer
  "glp" 'sdcv-search-pointer+
  "glr" 'dictionary-overlay-toggle
  "gls" 'emacs-azure-tts-sentence
  "glt" 'powerthesaurus-lookup-dwim
  "glv" 'lc-memo-review
  "glw" 'popweb-dict-say-word)

(evil-set-initial-state 'org-agenda-mode 'motion)

(evil-define-key 'motion org-agenda-mode-map
  (kbd "RET") 'org-agenda-switch-to
  "/" 'org-agenda-filter
  "SPC" 'nil
  "gj" 'org-agenda-next-item
  "gr" 'org-agenda-redo
  "gR" 'org-agenda-redo-all
  "t" 'org-agenda-todo
  "u" 'org-agenda-undo
  "I" 'org-agenda-clock-in
  "O" 'org-agenda-clock-out
  "cg" 'org-agenda-clock-goto
  "cc" 'org-agenda-clock-cancel
  "cr" 'org-agenda-clockreport-mode)
(evil-define-key 'normal 'beancount-mode-map
  "zf" 'beancount-fava)

(evil-define-key 'normal dired-mode-map
  "e" 'xah-show-in-desktop)

(evil-define-key 'normal achive-visual-mode-map
  "q" 'quit-window)

(evil-define-key 'normal sdcv-mode-map
  "q" 'quit-window)

(provide 'init-keybindings)
;;; init-keybindings.el ends here.
