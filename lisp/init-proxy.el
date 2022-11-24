;;; init-proxy.el ---  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(defun toggle-proxy ()
  "Toggle proxy for the url.el library."
  (interactive)
  (if url-proxy-services
      (proxy-disable)
    (proxy-enable)))

(defun proxy-enable ()
  "Enable proxy."
  (interactive)
  (setq url-proxy-services
        '(("http" . "127.0.0.1:8889")
          ("https" . "127.0.0.1:8889")
          ("no_proxy" . "0.0.0.0")))
  (message "Proxy enabled! %s" (car url-proxy-services)))

(defun proxy-disable ()
  "Disable proxy."
  (interactive)
  (if url-proxy-services
      (setq url-proxy-services nil))
  (message "Proxy disabled!"))

;; enable proxy before run these functions.
(advice-add 'gts-do-translate :before 'proxy-enable)
(advice-add 'lingva-translate :before 'proxy-enable)
(advice-add 'telega :before 'proxy-enable)
(advice-add 'magit-status :before 'proxy-enable)
(advice-add 'dictionary-overlay-start :before 'proxy-enable)

(provide 'init-proxy)
;;; init-proxy.el ends here
