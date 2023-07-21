;; init-keybindings.el --- Keybindings. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(one-key-create-menu
 "Applications"
 '(
   (("c" . "Calculator") . calc)
   )
 t)

(one-key-create-menu
 "Notes"
 '(
   (("n" . "Open Notes") . consult-notes)
   (("N" . "New Notes") . my/denote-signature-or-subdirectory)
   (("f" . "Node find") . org-roam-node-find)
   )
 t)
(global-set-key (kbd "C-c n") 'one-key-menu-notes)

(one-key-create-menu
 "Languages"
 '(
   (("l" . "Translate") . my/gts-do-translate)
   (("L" . "Lingva") . lingva-translate)
   (("p" . "sdcv pointer+") . sdcv-search-pointer+)
   (("P" . "sdcv pointer") . sdcv-search-pointer)
   (("h" . "sdcv helper") . lsp-bridge-toggle-sdcv-helper)
   (("r" . "Dictionary overlay toggle") . dictionary-overlay-toggle)
   (("w" . "Say word") . popweb-dict-say-word)
   )
 t)
;; (global-set-key (kbd "C-c l") 'one-key-menu-languages)

(one-key-create-menu
 "Search"
 '((("b" . "Browse remote") . browse-at-remote)
   (("f" . "Find file") . consult-find)
   (("l" . "Grab link") . my/link-grab)
   (("p" . "Emacs packages search") . epkg-describe-package)
   (("g" . "Google") . engine/search-google)
   (("w" . "Wikipedia") . engine/search-wikipedia)
   (("m" . "Movie") . engine/search-moviedouban)
   (("z" . "Zhihu") . engine/search-zhihu))
 t)
(global-set-key (kbd "<f4>") 'one-key-menu-search)

(evil-define-key 'visual 'evil-visual-state-map
  "v" 'er/expand-region)

(evil-define-key 'normal org-mode-map
  "gh" 'consult-outline)

(evil-define-key 'normal sdcv-mode-map
  "q" 'quit-window)

(evil-define-key '(normal visual motion) 'global
  "gb" 'tabspaces-switch-to-buffer
  "gs" 'tab-switch
  "gX" 'jf/org-link-remove-link
  "gF" 'embark-open-externally
  "gl" 'one-key-menu-languages)

(evil-define-key '(normal visual motion) dired-mode-map
    (kbd "RET") 'open-with-default-app
    "e" 'xah-show-in-desktop
    "p" 'dired-preview)

(evil-define-key '(normal visual) org-mode-map
  "zw" 'olivetti-mode)

(provide 'init-keybindings)
;;; init-keybindings.el ends here.
