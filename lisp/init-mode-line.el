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
