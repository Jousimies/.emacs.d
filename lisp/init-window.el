(use-package window
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
                                       (window-parameters
                                        (mode-line-format . none))))
  (add-to-list 'display-buffer-alist '((or (derived-mode . org-mode)
                                           (derived-mode . LaTeX-mode))
                                       (display-buffer-in-tab)
                                       (tab-name . "Edit") (tab-group . "Edit")
                                       (select . t)))

  (add-to-list 'display-buffer-alist '((derived-mode . prog-mode)
                                       (display-buffer-in-tab)
                                       (tab-name . "Porg") (tab-group . "Prog")
                                       (select . t))))
(use-package ace-window
  :bind ("C-x o" . ace-window))

(defun my/scroll-other-windown-down ()
  "Scroll other window down."
  (interactive)
  (scroll-other-window-down 2))

(global-set-key (kbd "M-p") 'my/scroll-other-windown-down)

(defun my/scroll-other-windown ()
  "Scroll other window up."
  (interactive)
  (scroll-other-window 2))

(global-set-key (kbd "M-n") 'my/scroll-other-windown)

(use-package ace-window
  :bind ("C-x o" . ace-window))

(provide 'init-window)
;;; init-window.el ends here.
