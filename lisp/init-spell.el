(use-package ispell
  :defer t
  :config
  (setq ispell-program-name "/opt/homebrew/bin/aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))
  (setq ispell-aspell-dict-dir
        (ispell-get-aspell-config-value "dict-dir"))

  (setq ispell-aspell-data-dir
        (ispell-get-aspell-config-value "data-dir"))

  (setq ispell-personal-dictionary (expand-file-name "config/ispell/.aspell.en.pws" my-galaxy))

  (setq-default ispell-following-word t
                ispell-quietly t))

(use-package flyspell
  :hook (org-mode . flyspell-mode))

(use-package flyspell-correct
  :commands flyspell-correct-wrapper
  :bind ([remap flyspell-auto-correct-previous-word] . flyspell-correct-wrapper))

(provide 'init-spell)
;;; init-translate.el ends here.
