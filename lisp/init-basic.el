;; init-basic.el --- Basic. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package hydra
  :commands defhydra
  :config
  (setq hydra-hint-display-type 'posframe)
  (setq hydra-posframe-show-params `(:poshandler posframe-poshandler-frame-center
                                                 :internal-border-width 2
                                                 :internal-border-color "#61AFEF"
                                                 :left-fringe 16
                                                 :right-fringe 16)))

(use-package psearch
  :commands psearch-replace psearch-patch)

(use-package epkg
  :bind ("s-<f4>" . epkg-describe-package)
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
  :config
  (setq request-storage-directory (expand-file-name "cache/request" user-emacs-directory)))

(provide 'init-basic)
;;; init-basic.el ends here.
