;; init-core.el --- Core. -*- lexical-binding: t; no-byte-compile: t -*-

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
        '(("http" . "127.0.0.1:8118")
          ("https" . "127.0.0.1:8118")
          ("socks" . "127.0.0.1:8118")
          ("no_proxy" . "0.0.0.0")))
  (message "Proxy enabled! %s" (car url-proxy-services)))

(defun proxy-disable ()
  "Disable proxy."
  (interactive)
  (if url-proxy-services
      (setq url-proxy-services nil))
  (message "Proxy disabled!"))

(run-with-idle-timer 2 nil (lambda ()
                             (proxy-enable)))

(use-package psearch
  :commands psearch-replace psearch-patch)

(use-package epkg
  :bind ("C-c s p" . epkg-describe-package)
  :init
  (add-to-list 'display-buffer-alist '("^\\*Help\\*"
                                       (display-buffer-same-window)
                                       (side . right)
                                       (window-width . 0.5)
                                       (window-parameters
                                        (mode-line-format . none))))
  :config
  (setq epkg-repository (expand-file-name "cache/epkgs" user-emacs-directory)))

(use-package epkg-marginalia
  :defer t
  :config
  (cl-pushnew 'epkg-marginalia-annotate-package
              (alist-get 'package marginalia-annotator-registry)))

(use-package request
  :defer t
  :config
  (setq request-storage-directory (expand-file-name "cache/request" user-emacs-directory)))

(with-eval-after-load 'org
  (add-to-list 'org-options-keywords "AUTO_TANGLE:")

  (defun my/auto-tangle ()
    "Auto export blog."
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (goto-char 0)
        (if (string-equal (car
                           (cdr
                            (car
                             (org-collect-keywords '("AUTO_TANGLE")))))
                          "t")
            (org-babel-tangle)))))

  (add-hook 'after-save-hook 'my/auto-tangle))

(use-package diminish
  :commands diminish)

(provide 'init-core)
;;; init-core.el ends here.
