;; init-mail.el --- E-mail management. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:
;; 需要安装依赖：`mu' `isync' `msmtp'
;; 安装 mu 的同时会安装 mu4e，所以不需要在 Emacs 中安装 mu4e

;;; Code:

(use-package mm-encode
  :ensure nil
  :custom
  (mm-encrypt-option nil)
  (mm-sign-option nil))

(use-package message
  :ensure nil
  :hook (message-setup . message-sort-headers)
  :custom
  (message-mail-user-agent t)
  (compose-mail-user-agent-warnings nil)
  (message-confirm-send t)
  (message-kill-buffer-on-exit t)
  (message-kill-buffer-query nil)
  (message-wide-reply-confirm-recipients nil))

(setq user-full-name "Duan Ning")
(setq user-mail-address "duan_n@outlook.com")

(use-package auth-source
  :ensure nil
  :commands auth-source-pick-first-password)

(defvar mu4e-private (auth-source-pick-first-password :host "mail" :user "mail")
  "My private mail address.")

(use-package mu4e
  :load-path "/opt/homebrew/share/emacs/site-lisp/mu4e/"
  :commands mu4e
  :bind (:map mu4e-headers-mode-map
              ("C-c l" . org-store-link))
  :hook (mu4e-index-updated . (lambda ()
                                (mu4e-modeline-mode 1)))
  :custom
  (mail-user-agent 'mu4e-user-agent)
  (mu4e-main-hide-personal-addresses nil)
  (mu4e-mu-binary (executable-find "mu"))
  (mu4e-update-interval (* 15 60))
  (mu4e-get-mail-command (concat (executable-find "mbsync") " -a"))
  (mu4e-index-update-in-background t)
  (mu4e-index-update-error-warning t)
  (mu4e-index-update-error-warning nil)
  (mu4e-index-cleanup t)
  (mu4e-notification-support t)
  (mu4e-confirm-quit nil)
  (mu4e-attachment-dir "~/Downloads/")
  (mu4e-use-fancy-chars t)
  (mu4e-search-include-related t)
  (mu4e-headers-precise-alignment t)
  (mu4e-headers-include-related t)
  (mu4e-headers-auto-update t)
  (mu4e-headers-date-format "%d/%m/%y")
  (mu4e-headers-time-format "%H:%M")
  (mu4e-headers-fields '((:flags . 12)
                         (:human-date . 10)
                         (:subject . 100)
                         (:from-or-to . 40)
                         (:tags . 15)))
  (mu4e-compose-format-flowed nil)
  (mu4e-compose-signature-auto-include nil)
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-compose-reply-ignore-address `("no-?reply" ,user-mail-address))
  (mu4e-context-policy 'pick-first)
  (mu4e-bookmarks '((:name "Unread" :query "flag:unread AND NOT flag:trashed" :key ?u)
                    (:name "Last 7 days" :query "date:7d..now AND NOT flag:trashed" :key ?w)
                    (:name "Messages with images" :query "mime:image/*" :key ?p)))
  :config
  (setq mu4e-contexts `(,(make-mu4e-context
                          :name "outlook"
                          :enter-func
                          (lambda () (mu4e-message "Enter outlook context"))
                          :leave-func
                          (lambda () (mu4e-message "Leave outlook context"))
                          :match-func
                          (lambda (msg)
                            (when msg
                              (mu4e-message-contact-field-matches msg
                                                                  :to user-mail-address)))
                          :vars `((user-mail-address . ,user-mail-address)
                                  (mu4e-drafts-folder . "/outlook/Drafts")
                                  (mu4e-refile-folder . "/outlook/Archive")
                                  (mu4e-sent-folder . "/outlook/Sent")
                                  (mu4e-trash-folder . "/outlook/Deleted")
                                  (mu4e-maildir-shortcuts . ((:name "Archive" :maildir "/outlook/Archive" :key ?a)
                                                             (:name "Deleted" :maildir "/outlook/Deleted" :key ?d :hide t)
                                                             (:name "Draft" :maildir "/outlook/Draft" :key ?D)
                                                             (:name "Sent" :maildir "/outlook/Sent" :key ?s)
                                                             (:name "Junk" :maildir "/outlook/Junk" :key ?j)
                                                             (:name "Google Scholar" :maildir "/outlook/Google Scholar" :key ?g)))))
                        ,(make-mu4e-context
                          :name "seu"
                          :enter-func
                          (lambda () (mu4e-message "Enter seu context"))
                          :leave-func
                          (lambda () (mu4e-message "Leave seu context"))
                          :match-func
                          (lambda (msg)
                            (when msg
                              (mu4e-message-contact-field-matches msg
                                                                  :to mu4e-private)))
                          :vars `((user-mail-address . ,mu4e-private)
                                  (mu4e-drafts-folder . "/seu/&g0l6P3ux-")
                                  (mu4e-sent-folder . "/seu/&XfJT0ZAB-")
                                  (mu4e-trash-folder . "/seu/&XfJSIJZk-")
                                  (mu4e-maildir-shortcuts . ((:name "Inbox" :maildir "/seu/Inbox" :key ?i)
                                                             (:name "Junk" :maildir "/seu/&V4NXPpCuTvY-" :key ?j)
                                                             (:name "Deleted" :maildir "seu/&XfJSIJZk-" :key ?d :hide t)
                                                             (:name "Draft" :maildir "/seu/&g0l6P3ux-" :key ?D)
                                                             (:name "Sent" :maildir "/seu/&XfJT0ZAB-" :key ?s)))
                                  ))))
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
  (setq mu4e-headers-list-mark      '("s" . "󰕲 "))
  (setq mu4e-headers-personal-mark  '("p" . "󰸐 "))
  (setq mu4e-modeline-unread-items  '("U:" . "[U]"))
  (setq mu4e-modeline-new-items     '("N:" . "[N]"))
  (setq mu4e-modeline-all-read      '("R:" . "[R]"))
  (setq mu4e-modeline-all-clear     '("C:" . "[C]")))

(run-with-idle-timer 120 nil (lambda ()
                             (mu4e 'background)))

;; Send mail
;; (require 'smtpmail-async)
;; (setq send-mail-function 'async-sendmail-send-it
;;       message-send-mail-function 'async-smtpmail-send-it)
(use-package sendmail
  :ensure nil
  :after message
  :custom
  (sendmail-program (executable-find "msmtp"))
  (message-sendmail-envelope-from 'header)
  (send-mail-function 'message-send-mail-with-sendmail)
  (message-send-mail-function 'message-send-mail-with-sendmail))

(use-package org-msg
  :hook (mu4e-compose-pre . org-msg-mode)
  :custom
  (org-msg-options "html-preamble:nil html-postamble:nil toc:nil author:nil email:nil")
  (org-msg-greeting-fmt "\nHi%s,\n\n")
  (org-msg-recipient-names `(,user-mail-address . ,user-full-name))
  (org-msg-greeting-name-limit 3)
  (org-msg-default-alternatives '((new . (utf-8 html))
                                  (reply-to-text . (utf-8))
                                  (reply-to-html . (utf-8 html))))
  (org-msg-attached-file-reference
   "see[ \t\n]\\(?:the[ \t\n]\\)?\\(?:\\w+[ \t\n]\\)\\{0,3\\}\\(?:attached\\|enclosed\\)\\|\
(\\(?:attached\\|enclosed\\))\\|\
\\(?:attached\\|enclosed\\)[ \t\n]\\(?:for\\|is\\)[ \t\n]")
  (org-msg-convert-citation t)

  (org-msg-signature (concat "Best Regards,\n\n#+begin_signature\n*"
                             user-full-name
                             "*\n\n" (format-time-string "%Y-%m-%d")
                             "\n#+end_signature")))

(provide 'init-mail)
;;; init-mail.el ends here.
