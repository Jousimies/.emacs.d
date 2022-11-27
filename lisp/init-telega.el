;;; init-telega.el --- Telegram client.   -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(when (maybe-require-package 'telega)
  (setq telega-proxies (list '(:server "127.0.0.1" :port 8889 :enable t
                                       :type (:@type "proxyTypeHttp"))))
  (setq telega-server-libs-prefix "/opt/homebrew/Cellar/tdlib/1.8.0/include/")

  (setq telega-root-show-avatars nil)
  (setq telega-user-show-avatars nil)
  (setq telega-chat-show-avatars nil)

  (with-eval-after-load 'telega
    (require 'telega-alert)
    (telega-alert-mode))

  ;; activated input method.
  (add-hook 'evil-insert-state-entry-hook (lambda ()
                                            (if (eq major-mode 'telega-chat-mode)
                                                (activate-input-method "rime")))))


(provide 'init-telega)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-telega.el ends here
