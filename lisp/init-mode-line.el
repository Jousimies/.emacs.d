;; init-mode-line.el --- Mode line format. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(defun my/mode-line-padding ()
  (let* ((r-length (string-width (format-mode-line mode-line-misc-info))))
    (propertize " "
                'display `(space :align-to (- right ,(+ r-length 1))))))

(defun ntf/mode-line-format (left right)
  "Return a string of `window-width' length.
  Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width (- (window-width) (length left) 1)))
    (format (format "%%s %%%ds " available-width) left right)))

(setq mode-line-end-spaces
      '(""
        mode-line-misc-info
        ))

;; (add-to-list 'global-mode-string
;;              '(:eval (propertize
;;                       (concat
;;                        ;; "𝚻𝚨𝚩: "
;;                        "󱏈: "
;;                        (alist-get 'group (tab-bar--current-tab))) 'face 'font-lock-constant-face)))

(setq mode-line-position-column-line-format '(" %l:%c"))

;; (setq mode-line-percent-position '(-4 "%p"))

(setq-default
 mode-line-format
 '((:eval
    (ntf/mode-line-format
     ;; left portion
     (format-mode-line
      (quote ("%e"
              (:eval
               (when (bound-and-true-p evil-local-mode)
                 (propertize
                  (concat
                   " "
                   (upcase
                    (substring (symbol-name evil-state) 0 1))
                   (substring (symbol-name evil-state) 1))
                  'face 'mode-line-emphasis)))
              " " mode-line-frame-identification
              (:eval
               (if (buffer-modified-p)
                   (propertize (buffer-name) 'face 'font-lock-preprocessor-face)
                 mode-line-buffer-identification))
              " " mode-line-position
              (:eval
               (let ((sys (coding-system-plist buffer-file-coding-system)))
                 (if (memq (plist-get sys :category)
                           '(coding-category-undecided coding-category-utf-8))
                     "UTF-8"
                   (upcase (symbol-name (plist-get sys :name)))))))))
     ;; right portion
     (format-mode-line
      (quote
       (" " mode-line-misc-info
        (vc-mode vc-mode)
        " " (:eval (propertize
                    (concat
                     "󱏈 "
                     (alist-get 'group (tab-bar--current-tab))) 'face 'font-lock-constant-face))
        " " (:eval (when buffer-read-only
                     (concat "  "  (propertize "RO"
                                               'face 'font-lock-type-face
                                               'help-echo "Buffer is read-only"))))
        " " (:eval (propertize (if (listp mode-name)
                                   (car mode-name)
                                 mode-name) 'face 'font-lock-type-face))
        )))))))

(use-package mode-line-bell
  :defer 1
  :config
  (mode-line-bell-mode))

(provide 'init-mode-line)
;;; init-mode-line.el ends here.
