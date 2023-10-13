;; init-telega.el --- Telega. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package telega
  :load-path ("packages/telega.el/" "packages/rainbow-identifiers" "packages/visual-fill-column")
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
  (add-to-list 'display-buffer-alist '((or (derived-mode . telega-image-mode)
                                           (derived-mode . telega-webpage-mode)
                                           (derived-mode . image-mode))
                                       (display-buffer-in-tab)))

  (add-to-list 'display-buffer-alist '((derived-mode . telega-chat-mode)
                                       (display-buffer-in-side-window)
                                       (side . right)
                                       (window-width . 0.5)
                                       (window-parameters
                                        (mode-line-format . none)
                                        (select . t)))))

(provide 'init-telega)
;;; init-telega.el ends here.
