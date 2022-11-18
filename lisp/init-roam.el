;;; init-roam.el --- Note taking.  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
(when (maybe-require-package 'org-roam)
  (add-hook 'org-roam-mode-hook 'turn-on-visual-line-mode)
  (add-hook 'after-init-hook 'org-roam-db-autosync-enable)

  (setq org-roam-db-gc-threshold most-positive-fixnum)
  (setq org-roam-directory (file-truename (expand-file-name "roam" my-galaxy)))

  ;; There is another package org-roam-timestamp.
  (add-hook 'org-mode-hook (lambda ()
                             (setq-local time-stamp-active t
                                         time-stamp-start "#\\+MODIFIED:[ \t]*"
                                         time-stamp-end "$"
                                         time-stamp-format "\[%Y-%m-%d %3a %H:%M\]")
                             (add-hook 'before-save-hook 'time-stamp nil 'local)))

  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.25)))
  (defun tim/org-roam-buffer-show (_)
    (if (and
         ;; Don't do anything if we're in the minibuffer or in the calendar
         (not (minibufferp))
         (not (derived-mode-p 'calendar-mode))
         ;; Show org-roam buffer iff the current buffer has a org-roam file
         (xor (org-roam-file-p) (eq 'visible (org-roam-buffer--visibility))))
        (org-roam-buffer-toggle)))
  (add-hook 'window-buffer-change-functions 'tim/org-roam-buffer-show)
  ;; org-roam-capture
  (setq org-roam-capture-templates '(("a" "articles" plain "%?"
                                      :target (file+head "articles/${slug}.org"
                                                         "#+TITLE: ${title}\n#+CREATED: %U\n#+MODIFIED: \n")
                                      :unnarrowed t)
                                     ("b" "Books" plain (file "~/.emacs.d/template/readinglog")
                                      :target (file+head "books/${slug}.org"
                                                         "#+TITLE: ${title}\n#+CREATED: %U\n#+MODIFIED: \n")
                                      :unnarrowed t)
                                     ("d" "Diary" plain "%?"
                                      :target (file+datetree "daily/<%Y-%m>.org" day))
                                     ("m" "main" plain "%?"
                                      :target (file+head "main/${slug}.org"
                                                         "#+TITLE: ${title}\n#+CREATED: %U\n#+MODIFIED: \n")
                                      :unnarrowed t)
                                     ("p" "people" plain (file "~/.emacs.d/template/crm")
                                      :target (file+head "crm/${slug}.org"
                                                         "#+TITLE: ${title}\n#+CREATED: %U\n#+MODIFIED: \n")
                                      :unnarrowed t)
                                     ("r" "reference" plain (file "~/.emacs.d/template/reference")
                                      :target (file+head "ref/${citekey}.org"
                                                         "#+TITLE: ${title}\n#+CREATED: %U\n#+MODIFIED: \n")
                                      :unnarrowed t)
                                     ("w" "work" plain "%?"
                                      :target (file+head "work/${slug}.org"
                                                         "#+TITLE: ${title}\n#+CREATED: %U\n#+MODIFIED: \n")
                                      :unnarrowed t))))
(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "n" '(:ignore t :wk "Notes")
 "nb" '(org-roam-buffer-toggle :wk "Roam buffer")
 "nr" '(org-roam-node-random :wk "Random node")
 "nn" '(org-roam-node-find :wk "Find node")
 "ni" '(org-roam-node-insert :wk "Insert node")
 "ns" '(org-roam-db-sync :wk "Sync DB")

 "na" '(org-roam-alias-add :wk "Add alias")
 "nA" '(org-roam-alias-remove :wk "Remove alias")
 "nt" '(org-roam-tag-add :wk "Add tag")
 "nT" '(org-roam-tag-remove :wk "Remove tag")

 "nc" '(org-roam-dailies-capture-today :wk "Capture today")
 "nd" '(org-roam-dailies-goto-today :wk "Goto today")
 "nD" '(org-roam-dailies-goto-date :wk "Goto date"))

(with-eval-after-load 'org-roam
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))

  (cl-defmethod org-roam-node-directories ((node org-roam-node))
    (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
        (format "(%s)" (car (split-string dirs "/")))
      ""))

  (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
    (let* ((count (caar (org-roam-db-query
                         [:select (funcall count source)
                                  :from links
                                  :where (= dest $s1)
                                  :and (= type "id")]
                         (org-roam-node-id node)))))
      (format "[%d]" count)))

  (cl-defmethod org-roam-node-doom-filetitle ((node org-roam-node))
    "Return the value of \"#+title:\" (if any) from file that NODE resides in.
 If there's no file-level title in the file, return empty string."
    (or (if (= (org-roam-node-level node) 0)
            (org-roam-node-title node)
          (org-roam-get-keyword "TITLE" (org-roam-node-file node)))
        ""))

  (cl-defmethod org-roam-node-doom-hierarchy ((node org-roam-node))
    "Return hierarchy for NODE, constructed of its file title, OLP and direct title.
   If some elements are missing, they will be stripped out."
    (let ((title     (org-roam-node-title node))
          (olp       (org-roam-node-olp   node))
          (level     (org-roam-node-level node))
          (filetitle (org-roam-node-doom-filetitle node))
          (separator (propertize " > " 'face 'shadow)))
      (cl-case level
        ;; node is a top-level file
        (0 filetitle)
        ;; node is a level 1 heading
        (1 (concat (propertize filetitle 'face '(shadow italic))
                   separator title))
        ;; node is a heading with an arbitrary outline path
        (t (concat (propertize filetitle 'face '(shadow italic))
                   separator (propertize (string-join olp " > ") 'face '(shadow italic))
                   separator title)))))

  (setq org-roam-node-display-template (concat "${type:8} ${backlinkscount:3} ${doom-hierarchy:*}" (propertize "${tags:20}" 'face 'org-tag) " ")))

(when (maybe-require-package 'org-roam-ui)
  (setq org-roam-ui-sync-theme t)
  (setq org-roam-ui-follow t)
  (setq org-roam-ui-update-on-save t)
  (setq org-roam-ui-open-on-start t)

  (when (maybe-require-package 'websocket)
    (with-eval-after-load 'org-roam-ui
      (require 'websocket)))

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "nu" '(org-roam-ui-open :wk "Random node")))

(when (maybe-require-package 'consult-org-roam)
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "ns" '(consult-org-roam-search :wk "Search")
   "nl" '(:ignore t :wk "Open Links")
   "nlb" '(consult-org-roam-backlinks :wk "Backlinks")
   "nlf" '(consult-org-roam-forward-links :wk "Forward Links")))

(when (maybe-require-package 'consult-notes)
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "nv" '(consult-notes-org-roam-find-node-relation :wk "Node navigation")))

(when (maybe-require-package 'org-transclusion)
  (face-spec-set 'org-transclusion-fringe
                 '((((background light))
                    :foreground "black")
                   (t
                    :foreground "white"))
                 'face-override-spec)
  (face-spec-set 'org-transclusion-source-fringe
                 '((((background light))
                    :foreground "black")
                   (t
                    :foreground "white"))
                 'face-override-spec)

  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps 'org-mode-map
   :prefix "SPC m"
   :non-normal-prefix "M-SPC m"
   "t" '(:ignore t :wk "Transclusion")
   "ta" '(org-transclusion-add :wk "Add")
   "tA" '(org-transclusion-add-all :wk "Add all")
   "tr" '(org-transclusion-remove :wk "Remove")
   "tR" '(org-transclusion-remove-all :wk "Remove all")
   "tg" '(org-transclusion-refresh :wk "Refresh")
   "tm" '(org-transclusion-make-from-link :wk "Make link")
   "to" '(org-transclusion-open-source :wk "Open source")
   "te" '(org-transclusion-live-sync-start :wk "Edit live")))

;; dynamic agenda combine with org and org-roam.
;; (defun vulpea-agenda-files-update (&rest _)
;;   "Update the value of `org-agenda-files'."
;;   (setq org-agenda-files (seq-uniq
;;                           (append
;;                            (vulpea-project-files)
;;                            '("/path/to/file1"
;;                              "/path/to/file2"
;;                              "...")))))

(with-eval-after-load 'org-roam
  (defun vulpea-buffer-tags-get ()
    "Return filetags value in current buffer."
    (vulpea-buffer-prop-get-list "filetags" "[ :]"))
  (defun vulpea-buffer-tags-set (&rest tags)
    "Set TAGS in current buffer.
  If filetags value is already set, replace it."
    (if tags
        (vulpea-buffer-prop-set
         "filetags" (concat ":" (string-join tags ":") ":"))
      (vulpea-buffer-prop-remove "filetags")))
  (defun vulpea-buffer-tags-add (tag)
    "Add a TAG to filetags in current buffer."
    (let* ((tags (vulpea-buffer-tags-get))
           (tags (append tags (list tag))))
      (apply #'vulpea-buffer-tags-set tags)))
  (defun vulpea-buffer-tags-remove (tag)
    "Remove a TAG from filetags in current buffer."
    (let* ((tags (vulpea-buffer-tags-get))
           (tags (delete tag tags)))
      (apply #'vulpea-buffer-tags-set tags)))
  (defun vulpea-buffer-prop-set (name value)
    "Set a file property called NAME to VALUE in buffer file.
  If the property is already set, replace its value."
    (setq name (downcase name))
    (org-with-point-at 1
      (let ((case-fold-search t))
        (if (re-search-forward (concat "^#\\+" name ":\\(.*\\)")
                               (point-max) t)
            (replace-match (concat "#+" name ": " value) 'fixedcase)
          (while (and (not (eobp))
                      (looking-at "^[#:]"))
            (if (save-excursion (end-of-line) (eobp))
                (progn
                  (end-of-line)
                  (insert "\n"))
              (forward-line)
              (beginning-of-line)))
          (insert "#+" name ": " value "\n")))))
  (defun vulpea-buffer-prop-set-list (name values &optional separators)
    "Set a file property called NAME to VALUES in current buffer.
  VALUES are quoted and combined into single string using
  `combine-and-quote-strings'.
  If SEPARATORS is non-nil, it should be a regular expression
  matching text that separates, but is not part of, the substrings.
  If nil it defaults to `split-string-default-separators', normally
  \"[ \f\t\n\r\v]+\", and OMIT-NULLS is forced to t.
  If the property is already set, replace its value."
    (vulpea-buffer-prop-set
     name (combine-and-quote-strings values separators)))
  (defun vulpea-buffer-prop-get (name)
    "Get a buffer property called NAME as a string."
    (org-with-point-at 1
      (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                               (point-max) t)
        (buffer-substring-no-properties
         (match-beginning 1)
         (match-end 1)))))
  (defun vulpea-buffer-prop-get-list (name &optional separators)
    "Get a buffer property NAME as a list using SEPARATORS.
  If SEPARATORS is non-nil, it should be a regular expression
  matching text that separates, but is not part of, the substrings.
  If nil it defaults to `split-string-default-separators', normally
  \"[ \f\t\n\r\v]+\", and OMIT-NULLS is forced to t."
    (let ((value (vulpea-buffer-prop-get name)))
      (when (and value (not (string-empty-p value)))
        (split-string-and-unquote value separators))))
  (defun vulpea-buffer-prop-remove (name)
    "Remove a buffer property called NAME."
    (org-with-point-at 1
      (when (re-search-forward (concat "\\(^#\\+" name ":.*\n?\\)")
                               (point-max) t)
        (replace-match ""))))
  (defun vulpea-project-p ()
    "Return non-nil if current buffer has any todo entry.
TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
    (seq-find                                 ; (3)
     (lambda (type)
       (or (eq type 'todo)
           (eq type 'done)))
     (org-element-map                         ; (2)
         (org-element-parse-buffer 'headline) ; (1)
         'headline
       (lambda (h)
         (org-element-property :todo-type h)))))
  (defun vulpea-project-update-tag ()
    "Update PROJECT tag in the current buffer."
    (when (and (not (active-minibuffer-window))
               (vulpea-buffer-p))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (vulpea-buffer-tags-get))
               (original-tags tags))
          (if (vulpea-project-p)
              (setq tags (cons "project" tags))
            (setq tags (remove "project" tags)))
          ;; cleanup duplicates
          (setq tags (seq-uniq tags))
          ;; update tags if changed
          (when (or (seq-difference tags original-tags)
                    (seq-difference original-tags tags))
            (apply #'vulpea-buffer-tags-set tags))))))
  (defun vulpea-buffer-p ()
    "Return non-nil if the currently visited buffer is a note."
    (and buffer-file-name
         (string-prefix-p
          (expand-file-name (file-name-as-directory org-roam-directory))
          (file-name-directory buffer-file-name))))
  (defun vulpea-project-files ()
    "Return a list of note files containing 'project' tag." ;
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
                :from tags
                :left-join nodes
                :on (= tags:node-id nodes:id)
                :where (like tag (quote "%\"project\"%"))]))))

  (defun vulpea-agenda-files-update (&rest _)
    "Update the value of `org-agenda-files'."
    (setq org-agenda-files (seq-uniq
                            (append
                             (vulpea-project-files)
                             `(,(expand-file-name "todos/gtd.org" my-galaxy))))))

  (add-hook 'find-file-hook #'vulpea-project-update-tag)
  (add-hook 'before-save-hook #'vulpea-project-update-tag)
  (add-hook 'find-file-hook #'vulpea-agenda-files-update)

  (advice-add 'org-agenda :before #'vulpea-agenda-files-update)
  (advice-add 'org-todo-list :before #'vulpea-agenda-files-update))

(when (maybe-require-package 'citar)
  (when (maybe-require-package 'org-roam)
    (require-package 'citar-org-roam)
    (add-hook 'org-mode-hook 'citar-org-roam-mode)))

;; https://org-roam.discourse.group/t/opening-url-in-roam-refs-field/2564/4?u=jousimies
(defun gpc/open-node-roam-ref-url ()
  "Open the URL in this node's ROAM_REFS property, if one exists"
  (interactive)
  (when-let ((ref-url (org-entry-get-with-inheritance "ROAM_REFS")))
    (browse-url ref-url)))

(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "n" '(:ignore t :wk "Notes")
 "nlr" '(gpc/open-node-roam-ref-url :wk "Ref link"))

(provide 'init-roam)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-roam.el ends here
