(use-package elfeed
  :defer t
  :config
  (defun elfeed-display-buffer (buf &optional act)
    (pop-to-buffer buf '((display-buffer-reuse-window display-buffer-in-side-window)
                         (side . bottom)
                         (window-height . 0.8)
                         (reusable-frames . visible)
                         (window-parameters
                          (select . t)
                          (quit . t)
                          (popup . t)))))
  (setq elfeed-show-entry-switch #'elfeed-display-buffer))

(use-package elfeed-org
  :commands elfeed-org
  :general (my/space-leader-def
             "fe" '(my/rss-source :wk "Elfeed file"))
  :config
  (setq rmh-elfeed-org-files `(,(concat my-galaxy "/rss/elfeed.org")))
  (defun my/rss-source ()
    "Open elfeed config file."
    (interactive)
    (find-file (car rmh-elfeed-org-files))))

(use-package elfeed-summary
  :general (my/space-leader-def
             "E" '(elfeed-summary :wk "Elfeed"))
  :config
  (setq elfeed-summary-other-window t)
  (setq elfeed-summary-settings
        '((group (:title . "科技")
                 (:elements (query . (and tec (not emacs) (not blogs)))
                            (group (:title . "Emacs")
                                   (:elements (query . emacs))
                                   (:face . org-level-1))
                            (group (:title . "Blogs")
                                   (:elements (query . blogs)))))
          (group (:title . "News")
                 (:elements (query . news)))
          (group (:title . "Books")
                 (:elements (query . book)))
          (group (:title . "Finance")
                 (:elements (query . finance)))
          (group (:title . "Youtube")
                 (:elements (query . video)))))

  (advice-add 'elfeed-summary :after 'elfeed-summary-update)
  (advice-add 'elfeed-summary :before 'elfeed-org))

(provide 'init-elfeed)
;;; init-elfeed.el ends here.
