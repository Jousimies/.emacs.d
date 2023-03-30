;; init-mode-line.el --- Mode line format. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(defun my/mode-line-padding ()
  (let* ((r-length (string-width (format-mode-line mode-line-misc-info))))
    (propertize " "
                'display `(space :align-to (- right ,(+ r-length 1))))))

(setq mode-line-end-spaces
      '(""
        mode-line-misc-info
        ))

(add-to-list 'global-mode-string
             '(:eval (propertize
                      (concat
                       "𝚻𝚨𝚩: "
                       ;; (number-to-string (tab-bar--current-tab-index))
                       ;; ": "
                       (alist-get 'group (tab-bar--current-tab))) 'face 'font-lock-constant-face)))

(setq mode-line-position-column-line-format '(" %l,%c"))

(setq mode-line-percent-position '(-4 "%p"))

(setq-default mode-line-format
              `("%e"
                mode-line-front-space
                (:propertize ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote))
                mode-line-frame-identification
                mode-line-buffer-identification
                "  "
                (:eval (let ((sys (coding-system-plist buffer-file-coding-system)))
                         (if (memq (plist-get sys :category)
                                   '(coding-category-undecided coding-category-utf-8))
                             "UTF-8"
                           (upcase (symbol-name (plist-get sys :name))))))
                "  "
                mode-line-position
                "  "
                (vc-mode vc-mode)
                (:eval (when buffer-read-only
                         (concat "  "  (propertize "RO"
                                                   'face 'font-lock-type-face
                                                   'help-echo "Buffer is read-only"))))
                "  "
                (:eval (my/mode-line-padding))
                mode-line-end-spaces))

(use-package mode-line-bell
  :defer 1
  :config
  (mode-line-bell-mode))

(provide 'init-mode-line)
;;; init-mode-line.el ends here.
