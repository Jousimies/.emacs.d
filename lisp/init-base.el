;; init-base.el --- Base. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package one-key
  :commands one-key-create-menu
  :config
  (add-to-list 'display-buffer-alist
               '("^\\*One-Key\\*"
                 (display-buffer-in-side-window)
                 (window-height . 0.2)
                 (side . bottom))))

;;;###autoload
(one-key-create-menu
   "Applications"
   '((("c" . "Calculator") . calc)
     (("C" . "Calendar") . calendar)
     (("h" . "SDCV Helper") . lsp-bridge-toggle-sdcv-helper))
   t)

(use-package psearch
  :commands psearch-replace psearch-patch)

(use-package epkg
  :commands epkg-describe-package
  :init
  (add-to-list 'display-buffer-alist '("^\\*Help\\*"
                                       (display-buffer-in-tab)
                                       (side . right)
                                       (window-width . 0.5)
                                       (window-parameters
                                        (select . t)
                                        (mode-line-format . none))))
  :config
  (setq epkg-repository (expand-file-name "cache/epkgs" user-emacs-directory)))

(use-package epkg-marginalia
  :after (epkg marginalia)
  :config
  (cl-pushnew 'epkg-marginalia-annotate-package
              (alist-get 'package marginalia-annotator-registry)))

(use-package request
  :defer t
  :config
  (setq request-storage-directory (expand-file-name "cache/request" user-emacs-directory)))

(provide 'init-base)
;;; init-base.el ends here.
