(use-package winner
  :hook (on-first-buffer . winner-mode)
  :config
  (setq winner-dont-bind-my-keys t)
  (setq winner-boring-buffers '("*Completions*"
                                "*Compile-Log*"
                                "*inferior-lisp*"
                                "*Fuzzy Completions*"
                                "*Apropos*"
                                "*Help*"
                                "*cvs*"
                                "*Buffer List*"
                                "*Ibuffer*"
                                "*esh command on file*")))

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
  (add-to-list 'display-buffer-alist '((derived-mode . text-mode)
                                       (display-buffer-in-tab)
                                       (tab-name . "Edit") (tab-group . "Edit")
                                       (select . t)))

  (add-to-list 'display-buffer-alist '((derived-mode . prog-mode)
                                       (display-buffer-in-tab)
                                       (tab-name . "Porg") (tab-group . "Prog")
                                       (select . t))))

(use-package ace-window
  :bind ("C-x o" . ace-window))

(provide 'init-window)
;;; init-window.el ends here.
