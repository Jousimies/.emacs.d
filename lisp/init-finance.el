;;; init-finance.el --- Manage finance with org.  -*- lexical-binding: t no-byte-compile: t -*-
;;; Code:
;;; Commentary:
(when (maybe-require-package 'ledger-mode)

  ;; company-ledger is better than corfu.
  ;; (with-eval-after-load 'corfu
  ;;   (add-hook 'ledger-mode-hook 'corfu-mode))

  (when (maybe-require-package 'company-ledger)
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-ledger)))

  (setq ledger-schedule-file (expand-file-name "finance/schedule.ledger" my-galaxy))
  (setq ledger-reports
        '(("bal"            "%(binary) -f %(ledger-file) bal")
          ("bal this month" "%(binary) -f %(ledger-file) bal -p %(month) -S amount")
          ("bal this year"  "%(binary) -f %(ledger-file) bal -p 'this year'")
          ("net worth"      "%(binary) -f %(ledger-file) bal Assets Liabilities")
          ("account"        "%(binary) -f %(ledger-file) reg %(account)")
          ("reg" "%(binary) -f %(ledger-file) reg")
          ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")))

  (defun my/finance-file ()
    "Open finance file."
    (interactive)
    (find-file (expand-file-name "finance/finance.ledger" my-galaxy)))

  (general-define-key
   :keymaps '(normal visual emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "fo" '(:ignore t :wk "Open file")
   "fof" '(my/finance-file :wk "Finance file")))


(provide 'init-finance)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-finance.el ends here
