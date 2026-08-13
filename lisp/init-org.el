;; -*- lexical-binding: t; -*-

(with-eval-after-load 'org
    (setopt org-ellipsis " ⇲"
  	  org-modules '(org-habit)
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


(provide 'init-org)
