;; init-telega.el --- Telega. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package telega
  :load-path ("packages/telega.el/" "packages/rainbow-identifiers" "packages/visual-fill-column")
  :init
  (add-to-list 'display-buffer-alist '((or (derived-mode . telega-image-mode)
                                           (derived-mode . telega-webpage-mode)
                                           (derived-mode . image-mode))
                                       (display-buffer-in-tab)))
  (add-to-list 'display-buffer-alist '((derived-mode . telega-chat-mode)
                                       (display-buffer-in-side-window)
                                       (side . right)
                                       (window-width . 0.5)))
  :bind (("C-c T" . telega)
         (:map telega-chat-mode-map
               ("C-g" . quit-window)))
  :config
  (setf (alist-get 2 telega-avatar-factors-alist) '(0.45 . 0.1))
  (setq telega-chat-fill-column 78)
  (setq telega-completing-read-function completing-read-function)
  (setq telega-server-libs-prefix "/opt/homebrew/opt/tdlib/")
  (setq telega-proxies
        (list
         '(:server "127.0.0.1" :port 1080 :enable t
                   :type (:@type "proxyTypeSocks5"))))
  (defvar +tab-bar-telega-indicator-cache nil)
  (defun +tab-bar-telega-icon-update (&rest rest)
      (setq +tab-bar-telega-indicator-cache
            (when (and (fboundp 'telega-server-live-p)
                       (telega-server-live-p)
                       (buffer-live-p telega-server--buffer))
              (let* ((me-user (telega-user-me 'locally))
                     (online-p (and me-user (telega-user-online-p me-user)))
                     (mentioned-chats (telega-filter-chats telega--ordered-chats '(mention)))
                     (mentioned-count (apply '+ (mapcar (telega--tl-prop :unread_mention_count) mentioned-chats)))
                     (unread-count (or (plist-get telega--unread-chat-count :unread_unmuted_count) 0))
                     (tot-count (+ mentioned-count unread-count)))
                (propertize (concat " "
                                    (if online-p "" "")
                                    (when (and tot-count (not (zerop tot-count)))
                                      (concat " " (number-to-string (+ unread-count mentioned-count))))
                                    " ")
                            'face `(:inherit ,(if online-p 'success 'warning)))))))

  (defun +tab-bar-telega-icon ()
    (or +tab-bar-telega-indicator-cache
        (+tab-bar-telega-icon-update)))

  (add-to-list 'tab-bar-format '+tab-bar-telega-icon t)

  (advice-add 'telega--on-updateUnreadChatCount :after #'+tab-bar-telega-icon-update)
  (add-hook 'telega-connection-state-hook #'+tab-bar-telega-icon-update)
  (add-hook 'telega-kill-hook #'+tab-bar-telega-icon-update))

(provide 'init-telega)
;;; init-telega.el ends here.
