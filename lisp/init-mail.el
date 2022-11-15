;;; init-mail.el --- Mail.   -*- lexical-binding: t no-byte-compile: t -*-
;;; Code:
;;; Commentary:
(setq display-time-mail-icon `(,(propertize (all-the-icons-material "mail")
                                            'face `(:family ,(all-the-icons-material-family)))))
(setq message-sendmail-envelope-from 'header)
(setq message-kill-buffer-query nil)
(setq message-sendmail-extra-arguments '("-a" "outlook"))
(setq message-send-mail-function 'sendmail-send-it)


(setq send-mail-function 'sendmail-send-it)
(setq sendmail-program (executable-find "msmtp"))
(setq mail-specify-envelope-from t)
(setq mail-envelope-from 'header)

(unless (fboundp 'mu4e)
  (autoload #'mu4e "mu4e" nil t))

(run-with-idle-timer 5 nil #'(lambda () (mu4e 'background)))

(with-eval-after-load 'mu4e
  (setq mu4e-mu-binary "/opt/homebrew/bin/mu")
  (setq mail-user-agent 'mu4e-user-agent)
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
  (setq mu4e-headers-fields '((:flags . 12)
                              (:human-date . 9)
                              (:subject . 90)
                              (:from-or-to . 40)
                              (:tags . 20)))
  (setq mu4e-use-fancy-chars nil)
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
  (setq mu4e-compose-dont-reply-to-self t)
  (with-eval-after-load 'mu4e-headers
    (setq mu4e-use-fancy-chars t)
    (setq mu4e-headers-list-mark `("s" . ,(propertize
                                           (all-the-icons-material "list")
                                           'face `(:family ,(all-the-icons-material-family)))))
    (setq mu4e-headers-seen-mark `("S" . ,(propertize
                                           (all-the-icons-material "mail_outline")
                                           'face `(:family ,(all-the-icons-material-family)))))

    (setq mu4e-headers-new-mark `("N" . ,(propertize
                                          (all-the-icons-material "markunread")
                                          'face `(:family ,(all-the-icons-material-family)))))

    (setq mu4e-headers-unread-mark `("u" . ,(propertize
                                             (all-the-icons-material "notifications_none")
                                             'face `(:family ,(all-the-icons-material-family)))))
    (setq mu4e-headers-signed-mark `("s" . ,(propertize
                                             (all-the-icons-material "check")
                                             'face `(:family ,(all-the-icons-material-family)))))

    (setq mu4e-headers-encrypted-mark `("x" . ,(propertize
                                                (all-the-icons-material "enhanced_encryption")
                                                'face `(:family ,(all-the-icons-material-family)))))

    (setq mu4e-headers-draft-mark `("D" . ,(propertize
                                            (all-the-icons-material "drafts")
                                            'face `(:family ,(all-the-icons-material-family)))))
    (setq mu4e-headers-attach-mark `("a" . ,(propertize
                                             (all-the-icons-material "attachment")
                                             'face `(:family ,(all-the-icons-material-family)))))
    (setq mu4e-headers-passed-mark `("P" . ,(propertize ; ❯ (I'm participated in thread)
                                             (all-the-icons-material "center_focus_weak")
                                             'face `(:family ,(all-the-icons-material-family)))))
    (setq mu4e-headers-flagged-mark `("F" . ,(propertize
                                              (all-the-icons-material "flag")
                                              'face `(:family ,(all-the-icons-material-family)))))
    (setq mu4e-headers-replied-mark `("R" . ,(propertize
                                              (all-the-icons-material "reply_all")
                                              'face `(:family ,(all-the-icons-material-family)))))
    (setq mu4e-headers-trashed-mark `("T" . ,(propertize
                                              (all-the-icons-material "cancel")
                                              'face `(:family ,(all-the-icons-material-family)))))

    (setq mu4e-headers-personal-mark `("p" . ,(propertize
                                               (all-the-icons-material "person")
                                               'face `(:family ,(all-the-icons-material-family)
                                                               :foreground 'mu4e-special-header-value-face)))))

  (when (maybe-require-package 'mu4e-alert)
    (add-hook 'mu4e-index-updated-hook 'mu4e-alert-enable-notifications)
    (mu4e-alert-enable-mode-line-display))

  (when (maybe-require-package 'mu4e-column-faces)
    (mu4e-column-faces-mode))

  (when (maybe-require-package 'mu4e-conversation)
    (global-mu4e-conversation-mode)))

(general-define-key
 :states '(normal visual emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "M" '(mu4e :wk "MAIL"))



(provide 'init-mail)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-mail.el ends here
