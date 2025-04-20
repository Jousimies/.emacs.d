;; init-finance.el --- Finance manager. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package ledger-mode
  :load-path "packages/ledger-mode/"
  :mode ("\\.ledger\\'" . ledger-mode)
  :config
  (setq ledger-accounts-file "~/Finance/main.txt")
  (setq ledger-reports '(("1-资产与负债表" "%(binary) -f %(ledger-file) bal ^Assets: ^Liabilities: -X CNY")
			 ("2-收入与支出表" "%(binary) -f %(ledger-file) bal ^Income: ^Expenses:")
			 ("3-预算表（本月）" "%(binary) -f %(ledger-file) budget --period \"this month\"")
			 ("4-投资回报" "%(binary) -f %(ledger-file) bal ^Assets:Investment -X CNY")
			 ("5-现金流动性" "%(binary) -f %(ledger-file) bal ^Assets:Bank")
			 ("6-交易日志（本月）" "%(binary) -f %(ledger-file) reg")
			 ("t1-今日支出总额" "%(binary) -f %(ledger-file) reg ^Expenses: --period today")
			 ("t2-本周支出总额" "%(binary) -f %(ledger-file) reg ^Expenses: --period \"this week\"")
			 ("t3-上周支出总额" "%(binary) -f %(ledger-file) reg ^Expenses: --period \"last week\"")
			 ("7-account" "%(binary) -f %(ledger-file) reg %(account)")
			 ("8-payee" "%(binary) -f %(ledger-file) reg @%(payee)"))))

(defun finance-sync-and-push ()
  "Pull repository, copy files from iCloud Finance to ~/Finance, and auto git push if changes exist."
  (interactive)
  (let* ((icloud-dir "~/Library/Mobile Documents/com~apple~CloudDocs/Finance/")
         (target-dir "~/Finance/")
         (files '("main.txt" "budget.ledger" "fund.ledger"))
         (commit-message (format-time-string "Finance update %Y-%m-%d %H:%M:%S")))
    ;; Ensure target directory exists
    (unless (file-exists-p target-dir)
      (make-directory target-dir t))
    ;; Pull repository first
    (shell-command (concat "cd " target-dir " && git pull origin main"))
    ;; Copy files
    (dolist (file files)
      (let ((src (expand-file-name file icloud-dir))
            (dst (expand-file-name file target-dir)))
        (when (file-exists-p src)
          (copy-file src dst t)
          (message "Copied %s to %s" file target-dir))))
    ;; Save all buffers
    (save-some-buffers t)
    ;; Check for changes using git status
    (let ((git-status (shell-command-to-string (concat "cd " target-dir " && git status --porcelain"))))
      (if (string-empty-p git-status)
          (message "No changes detected in Finance repository.")
        ;; Perform git operations if changes exist
        (shell-command (concat "cd " target-dir " && git add ."))
        (shell-command (concat "cd " target-dir " && git commit -m \"" commit-message "\""))
        (shell-command (concat "cd " target-dir " && git push origin main"))
        (message "Finance files copied and pushed!")))))


;; (use-package beancount
;;   :load-path "packages/beancount-mode/"
;;   :mode ("\\.bean\\'" . beancount-mode)
;;   :hook ((beancount-mode . (lambda ()
;;                              (setq-local electric-indent-chars nil)))
;;          (beancount-mode . outline-minor-mode))
;;   :config
;;   (setq beancount-highlight-transaction-at-point t)
;;   ;; insert whole transaction instead of only insert date.
;;   ;; (defun my/beancount-insert-transaction (&optional days)
;;   ;;   "Start a new timestamped directive with date shifted by DAYS from today."
;;   ;;   (interactive "P")
;;   ;;   (unless (bolp) (newline))
;;   ;;   (insert (beancount--shift-current-date days) " * \"\" \"\"")
;;   ;;   (backward-char 4))

;;   ;; (advice-add 'beancount-insert-date :override 'my/beancount-insert-transaction)

;;   ;; Auto open browser after beancount-fava started.
;;   (defun my/browser-beancount-fava ()
;;     (if beancount--fava-process
;;         (browse-url "http://127.0.0.1:5000")))

;;   (advice-add 'beancount-fava :after 'my/browser-beancount-fava)

;;   ;; auto align transaction before save file.
;;   (defun my/beancount-align-transaction ()
;;     "Align visible region in current buffer."
;;     (save-excursion
;;       (indent-region (window-start) (window-end))))

;;   (add-hook 'before-save-hook (lambda ()
;;                                 (if (eq major-mode 'beancount-mode)
;;                                     (my/beancount-align-transaction)))))

;; Use double-entry-genertor generate beancount file from source date, provided by Alipay and Wechat.
;; (defvar my/finance (expand-file-name "finance" my-galaxy))
;; (defvar my/finance-source-data (expand-file-name "SourceData/" my/finance))
;; (defvar DEG/config-dir (expand-file-name "Config/" my/finance))
;; (defvar my/beancount-file (expand-file-name "AllBeans/" my/finance))
;; (defvar DEG-cli "/opt/homebrew/bin/double-entry-generator")

;; (defvar my/bean-regexp "\\([0-9]\\{8\\}[_-]\\([0-9]\\{6\\}\\|[0-9]\\{8\\}\\)\\)")

;; (defun my/bean-rename-source (file)
;;   (when (string-match my/bean-regexp file)
;;     (let ((new-file-path (concat my/finance-source-data "wechat_" (match-string 1 file) ".csv")))
;;       (rename-file file new-file-path)
;;       new-file-path)))

;; According account data get from alipy and wechat, the csv file name has similiar pattern.
;; Generaly, I get data at the first day of month, the csv file will as `20240101' or `20240201'.
;; (defvar finance-source-regexp (concat (format-time-string "%Y") "0[1-2]01"))

;; (defun my/bean-generate (file)
;;   (interactive (list (read-file-name "CSV transaction:"
;;                                      my/finance-source-data nil nil finance-source-regexp)))
;;   (let* ((file (if (string-match-p "alipay" file) file (my/bean-rename-source file)))
;; 		 (prefix (if (string-match-p "alipay" file) "alipay" "wechat"))
;; 		 (config (concat DEG/config-dir prefix ".yaml"))
;; 		 (output (concat my/beancount-file (file-name-base file) ".bean"))
;; 		 (provider (if (string= prefix "alipay") "" "--provider wechat")))
;; 	(shell-command (format "%s translate --config %s %s --output %s %s"
;;                            DEG-cli config provider output file))))

;;;###autoload
;; (defun my/beancount-fava ()
;;   "Start (and open) or stop the fava server."
;;   (interactive)
;;   (require 'beancount)
;;   (if beancount--fava-process
;;       (progn
;;         (delete-process beancount--fava-process)
;;         (setq beancount--fava-process nil)
;;         (message "Fava process killed"))
;;     (setq beancount--fava-process
;;           (start-process "fava" (get-buffer-create "*fava*") "fava"
;;                          (if (eq 'beancount-mode major-mode) (buffer-file-name)
;;                            (read-file-name "File to load: " my/finance nil nil nil))))
;;     (set-process-filter beancount--fava-process #'beancount--fava-filter)
;;     (message "Fava process started")))

;; (use-package gnuplot
;;   :load-path "~/.emacs.d/packages/gnuplot/"
;;   :mode ("\\.gp$" . gnuplot-mode)
;;   :commands gnuplot-context-sensitive-mode gnuplot-mode gnuplot-make-buffer)

(provide 'init-finance)
;;; init-finance.el ends here.
