(use-package window
  :defer t
  :config
  (add-to-list 'display-buffer-alist '("\\.pdf"
                                       (display-buffer-in-tab)
                                       (tab-name . "PDF") (tab-group . "PDF")))
  (add-to-list 'display-buffer-alist '("\\*Outline"
                                       (display-buffer-in-side-window)
                                       (side . right)
                                       (window-width . 0.5)
                                       (window-parameters
                                        (mode-line-format . none))))
  (add-to-list 'display-buffer-alist '("\\*toc\\*"
                                       (display-buffer-reuse-window display-buffer-in-side-window)
                                       (side . left)
                                       (window-parameters
                                        (mode-line-format . none)
                                        (delete-other-windows . t))))
  (add-to-list 'display-buffer-alist '("\\*Org Note\\*"
                                       (display-buffer-in-side-window)
                                       (side . right)
                                       (slot . 0)
                                       (window-width . 0.5)
                                       (window-parameters . ((no-other-window . t)
                                                             (no-delete-other-windows . t)))))
  (add-to-list 'display-buffer-alist '((or (derived-mode . org-mode)
                                           (devived-mode . LaTeX-mode))
                                       (display-buffer-in-tab)
                                       (tab-name . "Edit") (tab-group . "Edit")
                                       (select . t)))

  (add-to-list 'display-buffer-alist '((derived-mode . prog-mode)
                                       (display-buffer-in-tab)
                                       (tab-name . "Porg") (tab-group . "Prog")
                                       (select . t))))
(use-package ace-window
  :bind ("C-x o" . ace-window))

(use-package frame
  :config
  (face-spec-set 'window-divider
                 '((((background light))
                    :foreground "#000000")
                   (t
                    :foreground "#FFFFFF"))
                 'face-override-spec)
  (setq window-divider-default-bottom-width 1)
  (setq window-divider-default-places 'bottom-only)
  (window-divider-mode))

(use-package ace-window
  :bind ("C-x o" . ace-window))

(provide 'init-window)
;;; init-window.el ends here.
