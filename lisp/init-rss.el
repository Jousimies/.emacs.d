;;; init-rss.el --- RSS reader with elfeed. -*- lexical-binding: t no-byte-compile: t -*-
;;; Code:
(when (maybe-require-package 'elfeed)
  (defun elfeed-display-buffer (buf &optional act)
    (pop-to-buffer buf '((display-buffer-reuse-window display-buffer-in-side-window)
                         (side . bottom)
                         (window-height . 0.8)
                         (reusable-frames . visible)
                         (window-parameters
                          (select . t)
                          (quit . t)
                          (popup . t)))))

  (setq elfeed-show-entry-switch #'elfeed-display-buffer)

  (when (maybe-require-package 'elfeed-org)
    (advice-add 'elfeed-summary :before 'elfeed-org)
    (setq rmh-elfeed-org-files `(,(concat my-galaxy "/rss/elfeed.org"))))

  (when (maybe-require-package 'elfeed-summary)
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
                   (:elements (query . finance)))))

    (advice-add 'elfeed-summary :after 'elfeed-summary-update)

    (general-define-key
     :states '(normal visual emacs)
     :prefix "SPC"
     :non-normal-prefix "M-SPC"
     "E" '(elfeed-summary :wk "Elfeed"))))

  (defun my/rss-source ()
    "Open elfeed config file."
    (interactive)
    (find-file (car rmh-elfeed-org-files)))


  (with-eval-after-load 'evil
    (evil-set-initial-state 'elfeed-search-mode 'emacs)
    (evil-set-initial-state 'elfeed-show-mode 'emacs))


(setq newsticker-url-list
      '(("Planet Emacslife" "https://planet.emacslife.com/atom.xml")
        ("Mastering Emacs" "http://www.masteringemacs.org/feed/")
        ("Oremacs" "https://oremacs.com/atom.xml")
        ("EmacsCast" "https://pinecast.com/feed/emacscast")
        ("Emacs TIL" "https://emacstil.com/feed.xml")
        ("Emacs Reddit" "https://www.reddit.com/r/emacs.rss")))


(provide 'init-rss)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rss.el ends here
