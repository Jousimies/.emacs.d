;; init-mode-line.el --- Mode line format. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(defun ntf/mode-line-format (left right)
  "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width (- (window-width) (length left) 1)))
    (format (format "%%s %%%ds " available-width) left right)))

(setq-default mode-line-format
              '((:eval
                 (ntf/mode-line-format
                  ;; left portion
                  (format-mode-line
                   (quote ("%e"
                           " " (:eval
                                (if (buffer-modified-p)
                                    (propertize (buffer-name) 'face 'font-lock-preprocessor-face)
                                  mode-line-buffer-identification))
                           " " mode-line-position)))
                  ;; right portion
                  (format-mode-line
                   (quote
                    (" " mode-line-misc-info
                     (vc-mode vc-mode)
                     " " (:eval
                          (let ((sys (coding-system-plist buffer-file-coding-system)))
                            (if (memq (plist-get sys :category)
                                      '(coding-category-undecided coding-category-utf-8))
                                "UTF-8"
                              (upcase (symbol-name (plist-get sys :name))))))
                     " " (:eval (when buffer-read-only
                                  (propertize ""
                                              'face 'font-lock-warning-face
                                              'help-echo "Buffer is read-only")))
                     " " (:eval (propertize (if (listp mode-name)
                                                (car mode-name)
                                              mode-name) 'face 'font-lock-type-face)))))))))

(use-package mode-line-bell
  :defer 1
  :config
  (mode-line-bell-mode))

(provide 'init-mode-line)
;;; init-mode-line.el ends here.
