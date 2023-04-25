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

(defun fast-file-view-mode ()
  "Makes the buffer readonly and disables fontlock and other bells and whistles
   for faster viewing"
  (interactive)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (fundamental-mode)
  (font-lock-mode -1)
  (when (boundp 'anzu-mode)
    (anzu-mode -1)))

(defvar large-file-extensions '("pdf" "jpg" "eps" "png")
  "A list of file extensions.")

(defun large-find-file-hook ()
  "If a file is over a given size, make the buffer read only."
  (let* ((file-size (buffer-size))
         (file-name (buffer-file-name))
         (file-ext (file-name-extension file-name)))
    (when (and (> file-size (* 1024 1024))
               (not (member file-ext large-file-extensions)))
      (fast-file-view-mode))))

(add-hook 'find-file-hook 'large-find-file-hook)

(provide 'init-basic)
;;; init-basic.el ends here.
