(use-package evil
  :load-path "~/.emacs.d/packages/evil/"
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode))

(when (and (featurep 'evil) (featurep 'emt))
  (advice-add 'evil-forward-word-begin :override #'emt-forward-word)
  (advice-add 'evil-backward-word-begin :override #'emt-backward-word))

(when (and (featurep 'evil) (featurep 'jinx))
  (advice-add 'ispell-word :override #'jinx-correct))

(use-package evil-collection
  :load-path "~/.emacs.d/packages/evil-collection" "~/.emacs.d/packages/annalist.el/"
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :load-path "~/.emacs.d/packages/evil-surround/"
  :after evil
  :config
  (global-evil-surround-mode 1))

(provide 'init-evil)
