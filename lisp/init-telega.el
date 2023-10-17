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
  ;; https://github.com/roife/.emacs.d/blob/d53e35de36a0ff25cb538baf6afcbdc9e39858af/core/init-tabbar.el
  (defvar +tab-bar-telega-indicator-cache nil)
  (defun +tab-bar-telega-icon-update (&rest rest)
    (setq +tab-bar-telega-indicator-cache
          (when (and (fboundp 'telega-server-live-p)
                     (telega-server-live-p)
                     (buffer-live-p telega-server--buffer))
            (let* ((me-user (telega-user-me 'locally))
                   (online-p (and me-user (telega-user-online-p me-user)))
                   ;; reactions
                   (reactions-chats (telega-filter-chats telega--ordered-chats '(unread-reactions)))
                   (reactions-count (apply '+ (mapcar (telega--tl-prop :unread_reaction_count) reactions-chats)))
                   ;; mentioned
                   (mentioned-chats (telega-filter-chats telega--ordered-chats '(mention)))
                   (mentioned-count (apply '+ (mapcar (telega--tl-prop :unread_mention_count) mentioned-chats)))
                   ;; unread
                   (unmuted-count (or (plist-get telega--unread-chat-count :unread_unmuted_count) 0))
                   (mentioned-unmuted-chats (telega-filter-chats telega--ordered-chats '(and (mention) (unmuted))))
                   (true-unmuted-count (- unmuted-count (length mentioned-unmuted-chats)))
                   ;; tot
                   ;; (tot-count (+ true-unmuted-count mentioned-count reactions-count))
                   )
              (propertize (concat "  "
                                  (when (and true-unmuted-count (not (zerop true-unmuted-count)))
                                    (concat "●" (number-to-string true-unmuted-count) " "))
                                  (when (and mentioned-count (not (zerop mentioned-count)))
                                    (concat "@" (number-to-string mentioned-count) " "))
                                  (when (and reactions-count (not (zerop reactions-count)))
                                    (concat "❤" (number-to-string reactions-count) " ")))
                          'face `(:inherit ,(if online-p 'success 'warning) :inverse-video t))))))

  (defun +tab-bar-telega-icon ()
    (or +tab-bar-telega-indicator-cache
        (+tab-bar-telega-icon-update)))

  (add-to-list 'tab-bar-format '+tab-bar-telega-icon t)

  (advice-add 'telega--on-updateUnreadChatCount :after #'+tab-bar-telega-icon-update)
  (advice-add 'telega--on-updateChatUnreadMentionCount :after #'+tab-bar-telega-icon-update)
  (advice-add 'telega--on-updateChatUnreadReactionCount :after #'+tab-bar-telega-icon-update)

  (add-hook 'telega-connection-state-hook #'+tab-bar-telega-icon-update)
  (add-hook 'telega-kill-hook #'+tab-bar-telega-icon-update))

(provide 'init-telega)
;;; init-telega.el ends here.
