;; init-finance.el --- Finance manager. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package beancount
  :load-path "packages/beancount-mode/"
  :mode (".bean" . beancount-mode)
  :hook ((beancount-mode . (lambda ()
                             (setq-local electric-indent-chars nil)))
         (beancount-mode . outline-minor-mode))
  :config
  ;; insert whole transaction instead of only insert date.
  (defun my/beancount-insert-transaction (&optional days)
    "Start a new timestamped directive with date shifted by DAYS from today."
    (interactive "P")
    (unless (bolp) (newline))
    (insert (beancount--shift-current-date days) " * \"\" \"\"")
    (backward-char 4))

  (advice-add 'beancount-insert-date :override 'my/beancount-insert-transaction)

  ;; Auto open browser after beancount-fava started.
  (defun my/browser-beancount-fava ()
    (if beancount--fava-process
        (browse-url "http://127.0.0.1:5000")))

  (advice-add 'beancount-fava :after 'my/browser-beancount-fava)

  ;; auto align transaction before save file.
  (defun my/beancount-align-transaction ()
    "Align visible region in current buffer."
    (save-excursion
      (indent-region (window-start) (window-end))))

  (add-hook 'before-save-hook (lambda ()
                                (if (eq major-mode 'beancount-mode)
                                    (my/beancount-align-transaction)))))

;; Use double-entry-genertor generate beancount file from source date, provided by Alipay and Wechat.
(defvar my/finance (expand-file-name "finance" my-galaxy))
(defvar my/finance-source-data (expand-file-name "SourceData/" my/finance))
(defvar DEG/config-dir (expand-file-name "Config/" my/finance))
(defvar my/beancount-file (expand-file-name "AllBeans/" my/finance))
(defvar DEG-cli "/opt/homebrew/bin/double-entry-generator")

(defvar my/bean-regexp "\\([0-9]\\{8\\}[_-]\\([0-9]\\{6\\}\\|[0-9]\\{8\\}\\)\\)")

(defun my/bean-rename-source (file)
  (when (string-match my/bean-regexp file)
    (let ((new-file-path (concat my/finance-source-data "wechat_" (match-string 1 file) ".csv")))
      (rename-file file new-file-path)
      new-file-path)))

(defun my/bean-generate (file)
  (interactive (list (read-file-name "CSV transaction:"
                                     my/finance-source-data nil nil ".csv")))
  (let* ((file (if (string-match-p "alipay" file) file (my/bean-rename-source file)))
		 (prefix (if (string-match-p "alipay" file) "alipay" "wechat"))
		 (config (concat DEG/config-dir prefix ".yaml"))
		 (output (concat my/beancount-file (file-name-base file) ".bean"))
		 (provider (if (string= prefix "alipay") "" "--provider wechat")))
	(shell-command (format "%s translate --config %s %s --output %s %s"
                           DEG-cli config provider output file))))

;;;###autoload
(defun my/beancount-fava ()
  "Start (and open) or stop the fava server."
  (interactive)
  (require 'beancount)
  (if beancount--fava-process
      (progn
        (delete-process beancount--fava-process)
        (setq beancount--fava-process nil)
        (message "Fava process killed"))
    (setq beancount--fava-process
          (start-process "fava" (get-buffer-create "*fava*") "fava"
                         (if (eq 'beancount-mode major-mode) (buffer-file-name)
                           (read-file-name "File to load: " my/finance nil nil nil))))
    (set-process-filter beancount--fava-process #'beancount--fava-filter)
    (message "Fava process started")))

(provide 'init-finance)
;;; init-finance.el ends here.
