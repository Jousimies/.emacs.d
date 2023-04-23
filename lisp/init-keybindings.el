;; init-keybindings.el --- Keybindings. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package which-key
  :config
  (setq which-key-sort-order #'which-key-prefix-then-key-order)
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 10000)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode))

(my/space-leader-def
  "m" '(:ignore t :wk "Misc")
  "mc" '(gptel :wk "ChatGPT")
  "md" '(disk-usage :wk "Disk usage")
  "me" '(emacs-lisp-macroexpand :wk "Macro expand")
  "mi" '(file-info-show :wk "File info")
  "mh" '(lsp-bridge-toggle-sdcv-helper :wk "SDCV Helper" )
  "mj" '(mac-launchpad :wk "Jump to App")
  "mo" '(my/ocr :wk "OCR")
  "mp" '(toggle-proxy :wk "Proxy")
  "ms" '(achive :wk "Share")
  "my" '(yt-set-time :wk "Youtube link time")

  "b" '(:ignore t :wk "Buffer")
  "bs" '(scratch-buffer :wk "*scratch*")
  "bm" '(switch-to-message :wk "*message*")

  "c" '(calendar :wk "Calendar")
  "t" '(telega :wk "Telega")
  "v" '(toggle-vterm :wk "vterm")
  "f" '(my/hydra-open-file/body :wk "Files")

  "d" '(:ignore t :wk "Download")
  "dc" '(org-download-clipboard :wk "Clipboard")
  "dr" '(my/org-download-rename :wk "Rename(arg)")
  "ds" '(org-download-screenshot :wk "Screenshot")
  "dy" '(org-download-yank :wk "Yank")

  "p" '(:ignore t :wk "Preview")
  "pa" '(math-preview-all :wk "All")
  "pA" '(math-preview-clear-all :wk "Clear All")
  "pp" '(math-preview-at-point :wk "Point")
  "pP" '(math-preview-clear-at-point :wk "Clear Point")
  "pr" '(math-preview-region :wk "Region")
  "pR" '(math-preview-clear-region :wk "Clear Region")

  "s" '(:ignore t :wk "Search")
  "sb" '(engine/search-bookdouban :wk "Book")
  "sf" '(consult-find :wk "Files")
  "ss" '(engine/search-google :wk "Google")
  "sg" '(engine/search-github :wk "Github")
  "sw" '(engine/search-wikipedia :wk "Wiki")
  "sm" '(engine/search-moviedouban :wk "Movie")
  "sz" '(engine/search-zhihu :wk "Zhihu")
  "sr" '(rg :wk "rg")
  "sl" '(consult-git-grep :wk "git"))

(evil-define-key '(normal visual motion) 'global
  "gb" 'tabspaces-switch-to-buffer
  "gs" 'tab-switch

  "gX" 'jf/org-link-remove-link

  "gF" 'embark-open-externally

  ;; Langrage learning
  "glc" 'lc-corpus-capture-card
  "gld" 'osx-dictionary-search-pointer
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
  "glw" 'popweb-dict-say-word

  ;; Note taking
  "gna" 'my/new-article
  "gnm" 'my/new-meeting
  "gnc" 'my/biblio-lookup-crossref
  "gnf" 'my/citar-denote-find-ref-or-citation
  "gnn" 'consult-notes
  "gnN" 'denote-menu-list-notes
  "gnp" 'citar-open-files
  "gno" 'citar-denote-open-note
  "gns" 'my/denote-signature-or-subdirectory
  "gnw" 'org-roam-node-find

  ;; EKG for fleeting notes
  "ged" 'ekg-show-notes-for-today
  "gee" 'ekg-show-notes-with-tag
  "gea" 'ekg-show-notes-with-any-tags
  "geA" 'ekg-show-notes-with-all-tags
  "geb" 'ekg-browse-url
  "ger" 'ekg-rename-tag)

(evil-define-key 'normal ekg-notes-mode-map
  "A" 'ekg-notes-any-tags
  "B" 'ekg-notes-select-and-browse-url
  "a" 'ekg-notes-any-note-tags
  "b" 'ekg-notes-browse
  "c" 'ekg-notes-create
  "d" 'ekg-notes-delete
  "n" 'ekg-notes-next
  "o" 'ekg-notes-open
  "p" 'ekg-notes-previous
  "r" 'ekg-notes-remove
  "t" 'ekg-notes-tag)

(evil-define-key '(normal visual) org-mode-map
  "gnd" 'citar-denote-dwim

  "gnk" 'citar-denote-add-citekey
  "gnK" 'citar-denote-remove-citekey

  "gnr" 'denote-rename-file-using-front-matter

  "gni" 'my/org-insert-web-page-archive
  "gnb" 'denote-org-dblock-insert-backlinks
  "gnl" 'denote-link-or-create
  "gnL" 'denote-org-dblock-insert-links

  "gntm" 'org-transclusion-make-from-link
  "gnta" 'org-transclusion-add
  "gntA" 'org-transclusion-add-all
  "gntr" 'org-transclusion-remove
  "gntR" 'org-transclusion-remove-all
  "gntg" 'org-transclusion-refresh
  "gnto" 'org-transclusion-open-source
  "gnts" 'org-transclusion-live-sync-start)

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

(evil-define-key 'visual 'evil-visual-state-map
  "v" 'er/expand-region)

(evil-define-key 'normal 'beancount-mode-map
  "zf" 'beancount-fava)

(evil-define-key '(normal visual motion) dired-mode-map
  (kbd "RET") 'open-with-default-app
  "gnr" 'denote-dired-rename-marked-files
  "e" 'xah-show-in-desktop)

(evil-define-key 'normal achive-visual-mode-map
  "q" 'quit-window)

(evil-define-key 'normal sdcv-mode-map
  "q" 'quit-window)

(evil-define-key 'normal denote-menu-mode-map
  "//" 'denote-menu-filter
  "/k" 'denote-menu-filter-by-keyword
  "c" 'denote-menu-clear-filters)

(evil-define-key 'normal vterm-mode-map
  "q" 'quit-window)

;; (with-eval-after-load 'telega
;;   (evil-define-key 'normal telega-chat-mode-map
;;     "q" 'quit-window))

(evil-define-key 'normal telega-root-mode-map
  "gs" nil)

(evil-define-key 'normal telega-msg-button-map
  "SPC" nil)

(provide 'init-keybindings)
;;; init-keybindings.el ends here.
