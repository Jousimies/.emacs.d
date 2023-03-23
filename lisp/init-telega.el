(use-package telega
  :general (my/space-leader-def
             "T" '(telega :wk "Telega"))
  :config
  (setq telega-server-libs-prefix "/opt/homebrew/opt/tdlib/")
  (setq telega-proxies
        (list
         '(:server "127.0.0.1" :port 1080 :enable t
                   :type (:@type "proxyTypeSocks5"))))
  (add-to-list 'display-buffer-alist '((or (derived-mode . telega-chat-mode)
                                           (derived-mode . telega-root-mode))
                                       (display-buffer-in-tab)
                                       (tab-name . "Chat") (tab-group . "Chat")
                                       (select . t))))

(evil-define-key 'normal telega-root-mode-map
    "gs" nil)

(use-package telega-notifications
  :hook (telega-load . telega-notifications-mode))

(use-package telega-alert
  :hook (telega-load . telega-alert-mode))

(provide 'init-telega)
;;; init-telega.el ends here.
