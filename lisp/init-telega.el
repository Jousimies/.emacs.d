;;; init-telega.el --- Telegram client.   -*- lexical-binding: t no-byte-compile: t -*-
;;; Code:
;;; Commentary:
(when (maybe-require-package 'telega)
  (setq telega-proxies (list '(:server "127.0.0.1" :port 8889 :enable t
                                       :type (:@type "proxyTypeHttp"))))
  (setq telega-server-libs-prefix "/opt/homebrew/Cellar/tdlib/1.8.0/include/")

  (general-define-key
   :states '(normal visual emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "T" '(telega :wk "Telega")))

(provide 'init-telega)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-telega.el ends here
