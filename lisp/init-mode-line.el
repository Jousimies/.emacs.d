;; init-mode-line.el --- Mode line format. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package echo-bar
  :hook (after-init . echo-bar-mode)
  :config
  (setq echo-bar-minibuffer nil)
  (setq echo-bar-right-padding 1)
  (setq echo-bar-format '((:eval (if (buffer-modified-p)
                                     (propertize (buffer-name) 'face 'font-lock-preprocessor-face)
                                   mode-line-buffer-identification))
                          " " (:eval
                               (let ((sys (coding-system-plist buffer-file-coding-system)))
                                 (if (memq (plist-get sys :category)
                                           '(coding-category-undecided coding-category-utf-8))
                                     "UTF-8"
                                   (upcase (symbol-name (plist-get sys :name))))))
                          " " (:eval (propertize (if (listp mode-name)
                                                     (car mode-name)
                                                   mode-name) 'face 'font-lock-type-face))
                          (:eval (when buffer-read-only
                                   (propertize " "
                                               'face 'font-lock-warning-face
                                               'help-echo "Buffer is read-only"))))))

(use-package mode-line-bell
  :defer 1
  :config
  (mode-line-bell-mode))

(provide 'init-mode-line)
;;; init-mode-line.el ends here.
