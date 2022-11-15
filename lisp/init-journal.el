;;; init-journal.el --- Journal. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'org-journal)
  (setq org-journal-dir (expand-file-name "journals" my-galaxy))
  (setq org-journal-date-format "%Y-%m-%d")
  (setq org-journal-file-type 'yearly))

(general-define-key
 :states '(normal visual emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "J" '(org-journal-new-entry :wk "Journal"))

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'org-mode-map
 :prefix "SPC m"
 :non-normal-prefix "M-SPC m"
 "j" '(:ignore t :wk "Journal")
 "jc" '(org-journal-new-entry :wk "New entry")
 "jd" '(org-journal-new-date-entry :wk "New date entry")
 "jn" '(org-journal-next-entry :wk "Next entry")
 "jp" '(org-journal-previous-entry :wk "Previous entry")
 "js" '(org-journal-search :wk "Search")
 "jf" '(org-journal-search-forever :wk "Search forever")
 "jF" '(org-journal-search-future :wk "Search future")
 "jw" '(org-journal-search-calendar-week :wk "Search calendar week")
 "jm" '(org-journal-search-calendar-month :wk "Search calendar month")
 "jy" '(org-journal-search-calendar-year :wk "Search calendar year"))


(provide 'init-journal)
;;; init-journal.el ends here
