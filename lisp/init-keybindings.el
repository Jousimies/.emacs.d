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
  "gnts" 'org-transclusion-live-sync-start

  "zw" 'olivetti-mode)

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
  "e" 'xah-show-in-desktop
  "p" 'dired-preview)

(evil-define-key 'normal dirvish-mode-map
  "q" 'dirvish-quit)

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
;;     "q" 'quit-window)

;;   (evil-define-key 'normal telega-root-mode-map
;;     "gs" nil)

;;   (evil-define-key 'normal telega-msg-button-map
;;     "SPC" nil))
(evil-define-key 'normal dired-mode-map
  "/" 'consult-line)
(evil-define-key 'normal org-mode-map
  "gh" 'consult-outline)
(evil-define-key 'normal LaTeX-mode-map
  "gh" 'consult-outline)

(provide 'init-keybindings)
;;; init-keybindings.el ends here.
