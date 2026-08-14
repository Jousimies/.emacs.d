;; -*- lexical-binding: t; -*-

(with-eval-after-load 'org
  (setopt org-modules '(org-habit)
  	  org-imenu-depth 4
  	  org-return-follows-link t
  	  org-display-remote-inline-images 'download
  	  org-log-into-drawer t
  	  org-fast-tag-selection-single-key 'expert
  	  org-adapt-indentation nil
  	  org-fontify-quote-and-verse-blocks t
  	  org-support-shift-select t
  	  org-treat-S-cursor-todo-selection-as-state-change nil
  	  org-hide-leading-stars nil
  	  org-startup-indented nil
  	  org-startup-with-inline-images t
  	  org-image-actual-width nil
  	  org-use-speed-commands t
  	  org-highlight-latex-and-related '(latex script)
  	  org-enforce-todo-dependencies t
  	  org-enforce-todo-checkbox-dependencies t
  	  org-export-allow-bind-keywords t
  	  org-tags-sort-function 'org-string-collate-greaterp
  	  org-lowest-priority ?D
  	  org-priority-default ?C
  	  org-columns-default-format "%50ITEM %TODO %3PRIORITY %TAGS"
  	  org-persist-directory (expand-file-name "org-persist" cache-directory)))

;; ob-core
(with-eval-after-load 'ob-core
  (setq org-confirm-babel-evaluate nil)
  (defun my/org-babel-execute-src-block (&optional _arg info _params)
    "Load language if needed"
    (let* ((lang (nth 0 info))
           (sym (if (member (downcase lang) '("c" "cpp" "c++")) 'C (intern lang)))
           (backup-languages org-babel-load-languages))
      (unless (assoc sym backup-languages)
        (condition-case err
            (progn
              (org-babel-do-load-languages 'org-babel-load-languages (list (cons sym t)))
              (setq-default org-babel-load-languages (append (list (cons sym t)) backup-languages)))
          (file-missing
           (setq-default org-babel-load-languages backup-languages)
           err)))))
  (advice-add 'org-babel-execute-src-block :before 'my/org-babel-execute-src-block))


(provide 'init-org)
