(setq user-full-name "Duan Ning")
(setq user-mail-address "duan_n@outlook.com")

(use-package simple
  :defer t
  :config
  (setq mail-user-agent 'mu4e-user-agent))

(use-package message
  :defer t
  :config
  (setq message-kill-buffer-on-exit t)
  (setq message-kill-buffer-query nil)
  (setq message-sendmail-envelope-from 'header)
  (setq message-kill-buffer-query nil)
  (setq message-sendmail-extra-arguments '("-a" "outlook")))

(add-to-list 'load-path "/opt/homebrew/opt/mu/share/emacs/site-lisp/mu/mu4e")
(unless (fboundp 'mu4e)
  (autoload #'mu4e "mu4e" nil t))

(run-with-idle-timer 5 nil #'(lambda () (mu4e 'background)))

(with-eval-after-load 'mu4e
  (setq mu4e-mu-binary (executable-find "mu"))
  (setq mu4e-update-interval (* 15 60))
  (setq mu4e-attachment-dir "~/Downloads/")
  (setq mu4e-get-mail-command (concat (executable-find "mbsync") " -a"))
  (setq mu4e-index-update-in-background t)
  (setq mu4e-index-update-error-warning t)
  (setq mu4e-index-update-error-warning nil)
  (setq mu4e-index-cleanup t)
  (setq mu4e-view-show-images t)
  (setq mu4e-view-image-max-width 800)
  (setq mu4e-view-show-addresses t)
  (setq mu4e-confirm-quit nil)
  (setq mu4e-context-policy 'pick-first)
  (with-eval-after-load 'mu4e
    (setq mu4e-sent-folder   "/outlook/Sent"
          mu4e-drafts-folder "/outlook/Drafts"
          mu4e-trash-folder  "/outlook/Deleted"
          mu4e-refile-folder  "/outlook/Archive"))
  (setq mu4e-view-prefer-html nil)
  (setq mu4e-html2text-command 'mu4e-shr2text)
  (setq mu4e-main-hide-personal-addresses t)
  (setq mu4e-headers-precise-alignment t)
  (setq mu4e-headers-include-related t)
  (setq mu4e-headers-auto-update t)
  (setq mu4e-headers-date-format "%d/%m/%y")
  (setq mu4e-headers-time-format "%H:%M")
  (setq mu4e-headers-fields '((:flags . 4)
                              (:human-date . 9)
                              (:subject . 90)
                              (:from-or-to . 40)
                              (:tags . 20)))
  (setq mu4e-bookmarks '(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
                         ("date:today..now" "Today's messages" ?t)
                         ("date:7d..now" "Last 7 days" ?w)
                         ("date:1d..now AND NOT list:emacs-orgmode.gnu.org" "Last 1 days" ?o)
                         ("date:1d..now AND list:emacs-orgmode.gnu.org" "Last 1 days (org mode)" ?m)
                         ("maildir:/drafts" "drafts" ?d)
                         ("flag:flagged AND NOT flag:trashed" "flagged" ?f)
                         ("mime:image/*" "Messages with images" ?p)))
  (setq mu4e-compose-reply-ignore-address '("no-?reply" "duan_n@outlook.com"))
  (setq mu4e-compose-format-flowed nil)
  (setq mu4e-compose-signature-auto-include nil)
  (setq mu4e-compose-dont-reply-to-self t))

(with-eval-after-load 'mu4e
  (define-key mu4e-headers-mode-map (kbd "C-c l") 'org-store-link))

(my/space-leader-def
  "e" '(mu4e :wk "MAIL"))

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
(with-eval-after-load 'mu4e
  (add-to-list 'mu4e-view-actions '("print to PDF"  . extra-print-email-to-pdf)))

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

(with-eval-after-load 'mu4e
  (add-to-list 'mu4e-view-actions '("download as html"  . extra-save-email-html)))

(with-eval-after-load 'all-the-icons
  (setq display-time-mail-icon `(,(propertize
                                   (all-the-icons-material "mail")
                                   'face `(:family ,(all-the-icons-material-family))))))

(with-eval-after-load 'mu4e
  (setq mu4e-use-fancy-chars nil))

(use-package mu4e-alert
  :after mu4e
  :config
  (mu4e-alert-set-default-style 'osx-notifier)
  (mu4e-alert-enable-notifications)
  (mu4e-alert-enable-mode-line-display))

(use-package mu4e-column-faces
  :hook (mu4e-main-mode . mu4e-column-faces-mode))

(add-hook 'mu4e-main-mode-hook
          (lambda ()
            (progn
              (require 'smtpmail-async)
              (setq send-mail-function 'async-sendmail-send-it)
              (setq message-send-mail-function 'async-smtpmail-send-it))))
(setq sendmail-program (executable-find "msmtp"))
(setq mail-specify-envelope-from t)
(setq mail-envelope-from 'header)

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
