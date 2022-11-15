;;; init-proxy.el --- 	-*- lexical-binding: t no-byte-compile: t -*-
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

(run-with-idle-timer 1 nil (lambda ()
                             (proxy-enable)))

(general-define-key
 :keymaps '(normal visual emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "t" '(:ignore t :wk "Toggles")
 "tp" '(proxy-enable :wk "Enable proxy")
 "tP" '(proxy-disable :wk "Disable proxy"))

(provide 'init-proxy)
;;; init-proxy.el ends here
