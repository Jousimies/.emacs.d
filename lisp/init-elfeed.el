;; init-elfeed.el --- elfeed, rss reader *- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package elfeed
  :defer t
  :preface
  (defun elfeed-display-buffer (buf &optional act)
    (pop-to-buffer buf '((display-buffer-reuse-window display-buffer-in-side-window)
                         (side . bottom)
                         (window-height . 0.8)
                         (reusable-frames . visible)
                         (window-parameters
                          (select . t)
                          (quit . t)
                          (popup . t)))))
  :config
  (setq elfeed-show-entry-switch #'elfeed-display-buffer))

(add-to-list 'display-buffer-alist
             `(,(rx (| "*elfeed-search*"
                       "*elfeed-summary*"
                       "*elfeed-entry-"))
               (display-buffer-in-tab)
               (tab-name . "RSS")
               (tab-group . "RSS")
               (window-parameters . ((mode-line-format . none)))))

(use-package elfeed-org
  :after elfeed)

(setq rmh-elfeed-org-files `(,(concat my-galaxy "/rss/elfeed.org")))

(defun my/rss-source ()
    "Open elfeed config file."
    (interactive)
    (find-file (car rmh-elfeed-org-files)))

(my/space-leader-def
  "fe" '(my/rss-source :wk "Elfeed file"))

(use-package elfeed-summary
  :commands elfeed-summary
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

(my/space-leader-def
  "E" '(elfeed-summary :wk "Elfeed"))

(provide 'init-elfeed)
;;; init-elfeed.el ends here.
