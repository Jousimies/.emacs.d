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

;; Suppress start looking process.
;; https://github.com/company-mode/company-mode/issues/912
;; shut-up
(with-eval-after-load 'ispell
  (advice-add 'ispell-lookup-words :around
              (lambda (orig &rest args)
                (shut-up (apply orig args)))))

(use-package jinx
  :hook (org-mode . jinx-mode)
  ;; :bind ("C-c c" . jinx-correct)
  :bind (:map jinx-misspelled-map
              ("C-;" . jinx-correct))
  :config
  (add-to-list 'jinx-exclude-regexps '(t "\\cc"))
  (setq jinx-languages '("en" "fr" "de")))

(provide 'init-spell)
;;; init-spell.el ends here.
