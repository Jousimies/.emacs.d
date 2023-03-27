(setq user-full-name "Duan Ning")
(setq user-mail-address "duan_n@outlook.com")

(use-package mu4e
  :load-path "/opt/homebrew/share/emacs/site-lisp/mu4e/"
  :commands mu4e
  :general (my/space-leader-def
             "e" '(mu4e :wk "MAIL"))
  :config
  (setq mu4e-confirm-quit nil)
  (add-to-list 'display-buffer-alist '((or (derived-mode . mu4e-main-mode)
                                           (derived-mode . mu4e-view-mode)
                                           (derived-mode . mu4e-headers-mode))
                                       (display-buffer-in-tab)
                                       (tab-name . "Mail") (tab-group . "Mail")
                                       (select . t))))
(run-with-timer 2 nil (lambda ()
                        (mu4e 'background)))

(use-package mu4e-main
  :after mu4e
  :config
  (setq mu4e-main-hide-personal-addresses nil))

(use-package mu4e-server
  :after mu4e
  :config
  (setq mu4e-mu-binary (executable-find "mu")))

(use-package mu4e-update
  :after mu4e
  :config
  (setq mu4e-update-interval (* 15 60))
  (setq mu4e-get-mail-command (concat (executable-find "mbsync") " -a"))
  (setq mu4e-index-update-in-background t)
  (setq mu4e-index-update-error-warning t)
  (setq mu4e-index-update-error-warning nil)
  (setq mu4e-index-cleanup t))

(use-package mu4e-folders
  :after mu4e
  :config
  (setq mu4e-attachment-dir "~/Downloads/")
  (setq mu4e-sent-folder   "/outlook/Sent"
        mu4e-drafts-folder "/outlook/Drafts"
        mu4e-trash-folder  "/outlook/Deleted"
        mu4e-refile-folder  "/outlook/Archive"))

(use-package mu4e-view
  :after mu4e
  :config
  (evil-set-initial-state 'mu4e-view-mode 'normal)
  (evil-define-key 'normal mu4e-view-mode-map
    "a" 'mu4e-view-action
    "q" 'quit-window)

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
  (add-to-list 'mu4e-view-actions '("print to PDF"  . extra-print-email-to-pdf)))

(use-package mu4e-modeline
  :after mu4e
  :config
  (mu4e-modeline-mode))

(use-package mu4e-context
  :after mu4e
  :config
  (setq mu4e-context-policy 'pick-first))

(use-package mu4e-headers
  :after mu4e
  :config
  (evil-set-initial-state 'mu4e-headers-mode 'normal)
  (evil-define-key 'normal mu4e-headers-mode-map
    "q" 'quit-window)

  (setq mu4e-headers-precise-alignment t)
  (setq mu4e-headers-include-related t)
  (setq mu4e-headers-auto-update t)
  (setq mu4e-headers-date-format "%d/%m/%y")
  (setq mu4e-headers-time-format "%H:%M")
  (setq mu4e-headers-fields '((:flags . 8)
                              (:human-date . 10)
                              (:subject . 100)
                              (:from-or-to . 40)
                              (:tags . 15)))
  (setq mu4e-headers-unread-mark    '("u" . " "))
  (setq mu4e-headers-draft-mark     '("D" . " "))
  (setq mu4e-headers-flagged-mark   '("F" . " "))
  (setq mu4e-headers-new-mark       '("N" . " "))
  (setq mu4e-headers-passed-mark    '("P" . " "))
  (setq mu4e-headers-replied-mark   '("R" . " "))
  (setq mu4e-headers-seen-mark      '("S" . " "))
  (setq mu4e-headers-trashed-mark   '("T" . " "))
  (setq mu4e-headers-attach-mark    '("a" . " "))
  (setq mu4e-headers-encrypted-mark '("x" . " "))
  (setq mu4e-headers-signed-mark    '("s" . " "))
  (setq mu4e-headers-list-mark '("s" . " "))
  (setq mu4e-headers-personal-mark '("p" . " "))

  (define-key mu4e-headers-mode-map (kbd "C-c l") 'org-store-link))

(use-package mu4e-bookmarks
  :after mu4e
  :config
  (setq mu4e-bookmarks '(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
                         ("date:today..now" "Today's messages" ?t)
                         ("date:7d..now" "Last 7 days" ?w)
                         ("date:1d..now AND NOT list:emacs-orgmode.gnu.org" "Last 1 days" ?o)
                         ("date:1d..now AND list:emacs-orgmode.gnu.org" "Last 1 days (org mode)" ?m)
                         ("maildir:/drafts" "drafts" ?d)
                         ("flag:flagged AND NOT flag:trashed" "flagged" ?f)
                         ("mime:image/*" "Messages with images" ?p))))

(use-package mu4e-draft
  :after mu4e
  :config
  (setq mu4e-compose-format-flowed nil)
  (setq mu4e-compose-signature-auto-include nil)
  (setq mu4e-compose-dont-reply-to-self t))

(use-package mu4e-contacts
  :after mu4e
  :config
  (setq mu4e-compose-reply-ignore-address '("no-?reply" "duan_n@outlook.com")))



(use-package mu4e-compose
  :after mu4e
  :config
  (setq mail-user-agent 'mu4e-user-agent))

(use-package mu4e-helpers
  :after mu4e
  :config
  (setq mu4e-use-fancy-chars t))

(use-package sendmail
  :config
  (setq sendmail-program (executable-find "msmtp"))
  (setq mail-specify-envelope-from t)
  (setq mail-envelope-from 'header))

(use-package org-msg
  :hook (mu4e-main-mode . org-msg-mode)
  :config
  (setq org-msg-options "html-preamble:nil html-postamble:nil toc:nil author:nil email:nil")
  (setq org-msg-greeting-fmt "\nHi%s,\n\n")
  (setq org-msg-recipient-names `(,user-mail-address . ,user-full-name))
  (setq org-msg-greeting-name-limit 3)
  (setq org-msg-default-alternatives '((new		. (text html))
                                       (reply-to-html	. (text html))
                                       (reply-to-text	. (text))))
  (setq org-msg-convert-citation t)

  (setq org-msg-signature (concat "Best Regards,\n\n#+begin_signature\n*"
                                  user-full-name
                                  "*\n\n" (format-time-string "%Y-%m-%d")
                                  "\n#+end_signature")))

(provide 'init-mail)
;;; init-mail.el ends here.
