;; init-mail.el --- E-mail management. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(setq user-full-name "Duan Ning")

(defvar mu4e-outlook "duan_n@outlook.com"
  "My outlook mail address.")
(use-package auth-source
  :commands auth-source-pick-first-password)
(defvar mu4e-gmail (auth-source-pick-first-password :host "mu4e" :user "gmail")
  "My gmail mail address.")

(use-package mu4e
  :load-path "/opt/homebrew/share/emacs/site-lisp/mu4e/"
  :commands mu4e
  :config
  ;; mu4e-main
  (setq mu4e-main-hide-personal-addresses nil)
  ;; mu4e-server
  (setq mu4e-mu-binary (executable-find "mu"))
  ;; mu4e-update
  (setq mu4e-update-interval (* 15 60))
  (setq mu4e-get-mail-command (concat (executable-find "mbsync") " -a"))
  (setq mu4e-index-update-in-background t)
  (setq mu4e-index-update-error-warning t)
  (setq mu4e-index-update-error-warning nil)
  (setq mu4e-index-cleanup t)
  ;; mu4e-view
  (defun extra-email-to-pdf (msg &optional args)
    "Pdf temp file MSG to a new name with ARGS ignored."
    (let* ((async-shell-command-display-buffer nil)
           (temp (format-time-string (expand-file-name "%Y-%m-%dT%H:%M.pdf" mail-source-directory)))
           (name (read-string "File name: " temp))
           (html (replace-regexp-in-string (regexp-quote "file://") "" msg t t)))
      (if args (message "Additional optional argument was ignored when saving to PDF."))
      (async-shell-command (concat "pandoc " html " -o " name))))

  (defun extra-print-email-to-pdf (msg &optional skip-headers)
    "Save current MSG as a pdf if it includes an HTML-part.
    If SKIP-HEADERS is set, do not show include message headers."
    (let* ((browse-url-browser-function  'extra-email-to-pdf))
      (mu4e-action-view-in-browser msg skip-headers)))

  (defun extra-move-temp-email-location (msg &optional args)
    "Move and rename temp file MSG to a new location with ARGS ignored."
    (let* ((temp (format-time-string (expand-file-name "html/%Y-%m-%dT%H:%M.html" mail-source-directory)))
           (name (read-string "File name: " temp))
           (file (replace-regexp-in-string (regexp-quote "file://") "" msg t t)))
      (if args (message "Additional optional argument was ignored when saving to HTML."))
      (rename-file file name)))

  (defun extra-save-email-html (msg &optional skip-headers)
    "Save current MSG HTML-part.
    If SKIP-HEADERS is set, do not show include message headers."
    (let* ((extra-temp-email-dir (expand-file-name "html" mail-source-directory))
           (browse-url-browser-function  'extra-move-temp-email-location))
      (mu4e-action-view-in-browser msg skip-headers)))

  (add-to-list 'mu4e-view-actions '("download as html"  . extra-save-email-html))
  (add-to-list 'mu4e-view-actions '("print to PDF"  . extra-print-email-to-pdf))
  ;; mu4e-vars
  (setq mu4e-notification-support t)
  (setq mu4e-confirm-quit nil)
  ;; mu4e-modeline
  (mu4e-modeline-mode 1)
  (add-to-list 'my/tab-bar-right-string '((:eval (mu4e--modeline-string))))
  ;; mu4e-folders
  (setq mu4e-maildir-shortcuts
        '(("/outlook/INBOX" . ?o)
          ("/outlook/Sent Messages" . ?O)
          ("/[Gmail]/INBOX" . ?g)
          ("/[Gmail]/Sent Mail" . ?G)))
  (setq mu4e-attachment-dir "~/Downloads/")
  ;; mu4e-helpers
  (setq mu4e-use-fancy-chars t)
  ;; mu4e-headers
  (setq mu4e-headers-precise-alignment t)
  (setq mu4e-headers-include-related t)
  (setq mu4e-headers-auto-update t)
  (setq mu4e-headers-date-format "%d/%m/%y")
  (setq mu4e-headers-time-format "%H:%M")
  (setq mu4e-headers-fields '((:flags . 12)
                              (:human-date . 10)
                              (:subject . 100)
                              (:from-or-to . 40)
                              (:tags . 15)))
  (setq mu4e-headers-unread-mark    '("u" . "󱃚 "))
  (setq mu4e-headers-draft-mark     '("D" . "󰻤 "))
  (setq mu4e-headers-flagged-mark   '("F" . "󰃀 "))
  (setq mu4e-headers-new-mark       '("N" . "󰇮 "))
  (setq mu4e-headers-passed-mark    '("P" . " "))
  (setq mu4e-headers-replied-mark   '("R" . " "))
  (setq mu4e-headers-seen-mark      '("S" . " "))
  (setq mu4e-headers-trashed-mark   '("T" . " "))
  (setq mu4e-headers-attach-mark    '("a" . " "))
  (setq mu4e-headers-encrypted-mark '("x" . " "))
  (setq mu4e-headers-signed-mark    '("s" . " "))
  (setq mu4e-headers-list-mark '("s" . "󰕲 "))
  (setq mu4e-headers-personal-mark '("p" . "󰸐 "))

  (define-key mu4e-headers-mode-map (kbd "C-c l") 'org-store-link)
  ;; mu4e-bookmarks
  (setq mu4e-modeline-unread-items '("U:" . " "))
  (setq mu4e-modeline-new-items '("N:" . " "))
  (setq mu4e-modeline-all-clear '("C: " . " "))
  (setq mu4e-bookmarks '(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
                         ("date:today..now" "Today's messages" ?t)
                         ("flag:trashed" "Trashed" ?T)
                         ("date:7d..now" "Last 7 days" ?w)
                         ;; ("date:1d..now AND NOT list:emacs-orgmode.gnu.org" "Last 1 days" ?o)
                         ;; ("date:1d..now AND list:emacs-orgmode.gnu.org" "Last 1 days (org mode)" ?m)
                         ("maildir:/drafts" "drafts" ?d)
                         ("flag:flagged AND NOT flag:trashed" "flagged" ?f)
                         ("mime:image/*" "Messages with images" ?p)))
  ;; mu4e-drafts
  (setq mu4e-compose-format-flowed nil)
  (setq mu4e-compose-signature-auto-include nil)
  (setq mu4e-compose-dont-reply-to-self t)
  ;; mu4e-contacts
  (setq mu4e-compose-reply-ignore-address '("no-?reply" "duan_n@outlook.com"))
  ;; mu4e-compose
  (setq mail-user-agent 'mu4e-user-agent)
  ;; mu4e-context
  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "Outlook"
            :enter-func
            (lambda () (mu4e-message "Enter outlook context"))
            :leave-func
            (lambda () (mu4e-message "Leave outlook context"))
            :match-func
            (lambda (msg)
              (when msg
                (mu4e-message-contact-field-matches msg
                                                    :to 'mu4e-outlook)))
            :vars `((user-mail-address . ,mu4e-outlook)
                    (mu4e-drafts-folder . "/outlook/Drafts")
                    (mu4e-refile-folder . "/outlook/Archive")
                    (mu4e-sent-folder . "/outlook/Sent Messages")
                    (mu4e-trash-folder . "/outlook/Deleted Messages")))

          ,(make-mu4e-context
            :name "gmail"
            :enter-func
            (lambda () (mu4e-message "Enter gmail context"))
            :leave-func
            (lambda () (mu4e-message "Leave gmail context"))
            :match-func
            (lambda (msg)
              (when msg
                (mu4e-message-contact-field-matches msg
                                                    :to 'mu4e-gmail)))
            :vars `((user-mail-address . ,mu4e-gmail)
                    (mu4e-drafts-folder . "/gmail/Drafts")
                    (mu4e-refile-folder . "/gmail/Archive")
                    (mu4e-sent-folder . "/gmail/Sent")
                    (mu4e-trash-folder . "/gmail/Trash")))))
  (setq mu4e-context-policy 'pick-first))

(run-with-idle-timer 4 nil (lambda ()
                             (mu4e 'background)))

(use-package consult-mu
  :load-path "packages/consult-mu/"
  :after consult mu4e
  :bind ("s-f m" . consult-mu)
  :commands consult-mu)

(provide 'init-mail)
;;; init-mail.el ends here.
