;;; init-spell.el ---  spell check.  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'ispell
  (setq ispell-program-name "/opt/homebrew/bin/aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))
  (setq ispell-aspell-dict-dir
        (ispell-get-aspell-config-value "dict-dir"))

  (setq ispell-aspell-data-dir
        (ispell-get-aspell-config-value "data-dir"))

  (setq ispell-personal-dictionary (expand-file-name "config/ispell/.aspell.en.pws" my-galaxy))

  (setq-default ispell-following-word t
	            ispell-quietly t))

(when (maybe-require-package 'wucuo)
  (add-hook 'org-mode-hook #'wucuo-start))

(with-eval-after-load 'flyspell
  (define-key flyspell-mode-map (kbd "C-;") nil)
  (define-key flyspell-mode-map (kbd "C-,") nil)
  (define-key flyspell-mode-map (kbd "C-.") nil)
  (when (maybe-require-package 'flyspell-correct)
    (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-wrapper)))


(provide 'init-spell)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-spell.el ends here
