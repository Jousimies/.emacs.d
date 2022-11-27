;;; init-finance.el --- Manage finance with org.  -*- lexical-binding: t no-byte-compile: t -*-
;;; Code:
;;; Commentary:
;; Ledger sort data functions is useful.
(when (maybe-require-package 'ledger-mode)

  (setq ledger-schedule-file (expand-file-name "finance/schedule.ledger" my-galaxy))
  (setq ledger-reports
        '(("bal"            "%(binary) -f %(ledger-file) bal")
          ("bal this month" "%(binary) -f %(ledger-file) bal -p %(month) -S amount")
          ("bal this year"  "%(binary) -f %(ledger-file) bal -p 'this year'")
          ("net worth"      "%(binary) -f %(ledger-file) bal Assets Liabilities")
          ("account"        "%(binary) -f %(ledger-file) reg %(account)")
          ("reg" "%(binary) -f %(ledger-file) reg")
          ("payee" "%(binary) -f %(ledger-file) reg @%(payee)"))))

;; Beancount has more plugin than ledger-cli.
;; (require 'beancount)
(add-to-list 'auto-mode-alist '("\\.bean\\'" . beancount-mode))
(add-hook 'beancount-mode-hook
          (lambda () (setq-local electric-indent-chars nil)))
(add-hook 'beancount-mode-hook #'outline-minor-mode)

(with-eval-after-load 'evil-collection
    (evil-collection-define-key 'normal 'beancount-mode-map
      "zf" 'beancount-fava))


(provide 'init-finance)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-finance.el ends here
