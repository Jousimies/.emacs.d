;; init-elfeed.el --- RSS reader. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package elfeed
  :load-path "packages/elfeed/"
  :commands elfeed-entry-link
  :bind ((:map elfeed-search-mode-map
               ("o" . z/xwidget-webkit-browse-entry-link-at-point))
         (:map elfeed-show-mode-map
               ("o" . z/xwidget-webkit-browse-entry-link-at-point)))
  :config
  (defun z/xwidget-webkit-browse-entry-link-at-point ()
    (interactive)
    (let ((entry-link
           (if (eq major-mode 'elfeed-search-mode)
               (elfeed-entry-link (elfeed-search-selected t))
             (elfeed-entry-link elfeed-show-entry))))
      (xwidget-webkit-browse-url entry-link)))
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
  :load-path "packages/elfeed-org/"
  :commands elfeed-org
  :config
  (setq rmh-elfeed-org-files `(,(concat my-galaxy "/denote/20230330T120149==5d2b3--rss-sources__elfeed_emacs.org"))))

(use-package elfeed-summary
  :load-path "packages/elfeed-summary/"
  :bind ("C-c E" . elfeed-summary)
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
