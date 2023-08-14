;; init-keybindings.el --- Keybindings. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(evil-define-key 'visual 'evil-visual-state-map
  "v" 'er/expand-region)

(evil-define-key 'normal org-mode-map
  "zw" 'olivetti-mode
  "gh" 'consult-outline
  "gli" 'my/link-grab)

(evil-define-key '(normal visual motion) 'global
  "gb" 'tabspaces-switch-to-buffer
  "gs" 'tab-switch)

(evil-define-key '(normal visual motion) dired-mode-map
  "e" 'xah-show-in-desktop
  "p" 'dired-preview)

(evil-define-key 'normal sdcv-mode-map
  "q" 'quit-window)

(evil-define-key '(normal visual motion) 'global
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
  "glw" 'popweb-dict-say-word)

(provide 'init-keybindings)
;;; init-keybindings.el ends here.
