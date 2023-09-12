;; init-org.el --- Live in plain life with org-mode. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package org
  :init
  (add-to-list 'display-buffer-alist '((derived-mode . org-mode)
                                       (display-buffer-in-tab)
                                       (tab-name . "Edit") (tab-group . "Edit")
                                       (select . t)))
  (add-to-list 'display-buffer-alist '("\\*Org Note\\*"
                                       (display-buffer-in-side-window)
                                       (side . bottom)
                                       (slot . 0)
                                       (window-width . 0.5)))
  :bind (:map org-mode-map
              ("C-c l" . org-store-link))
  :custom
  (org-ellipsis " ⇲")
  (org-modules '())
  (org-imenu-depth 4)
  (org-return-follows-link t)
  (org-image-actual-width nil)
  (org-display-remote-inline-images 'download)
  (org-log-into-drawer t)
  (org-fast-tag-selection-single-key 'expert)
  (org-adapt-indentation nil)
  (org-fontify-quote-and-verse-blocks t)
  (org-support-shift-select t)
  (org-treat-S-cursor-todo-selection-as-state-change nil)
  (org-hide-leading-stars nil)
  (org-startup-with-inline-images t)
  (org-startup-folded 'content)
  (org-image-actual-width '(500))
  (org-use-speed-commands t)
  (org-highlight-latex-and-related '(latex script))
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-tags-sort-function 'org-string-collate-greaterp)
  :config
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2))
  (setq org-todo-repeat-to-state t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "INPROGRESS(i)" "|" "WAIT(w@)" "SOMEDAY(s@)" "CNCL(c@/!)" "DONE(d)")))

  (setq org-todo-keyword-faces
        '(("TODO" . (:inherit (bold org-todo)))
          ("NEXT" . (:inherit (success org-todo)))
          ("CNCL" . (:inherit (shadow org-todo)))
          ("DONE" . (:inherit (button org-todo)))
          ("WAIT" . (:inherit (warning org-todo)))))

  (setq org-priority-faces
        '((?A . (bold . org-priority))
          (?B . org-priority)
          (?C . (shadow . org-priority))))

  (setq org-todo-state-tags-triggers
        (quote (("CNCL" ("CNCL" . t))
                ("WAIT" ("WAIT" . t))
                ("SOMEDAY" ("WAIT") ("SOMEDAY" . t))
                (done ("WAIT") ("SOMEDAY"))
                ("TODO" ("WAIT") ("CNCL") ("SOMEDAY"))
                ("NEXT" ("WAIT") ("CNCL") ("SOMEDAY"))
                ("DONE" ("WAIT") ("CNCL") ("SOMEDAY")))))
  ;;   此处的设置来源：https://protesilaos.com/emacs/modus-themes
  ;; 若升级 modus-themes 到 4.0 的版本，可能需要修改。
  (defface my-org-emphasis-bold
    '((default :inherit bold)
      (((class color) (min-colors 88) (background light))
       :foreground "#a60000")
      (((class color) (min-colors 88) (background dark))
       :foreground "#ff8059"))
    "My bold emphasis for Org.")

  (defface my-org-emphasis-italic
    '((default :inherit italic)
      (((class color) (min-colors 88) (background light))
       :foreground "#005e00")
      (((class color) (min-colors 88) (background dark))
       :foreground "#44bc44"))
    "My italic emphasis for Org.")

  (defface my-org-emphasis-underline
    '((default :inherit underline)
      (((class color) (min-colors 88) (background light))
       :foreground "#813e00")
      (((class color) (min-colors 88) (background dark))
       :foreground "#d0bc00"))
    "My underline emphasis for Org.")

  (defface my-org-emphasis-strike-through
    '((default :strike-through t)
      (((class color) (min-colors 88) (background light))
       :foreground "#505050")
      (((class color) (min-colors 88) (background dark))
       :foreground "#a8a8a8"))
    "My strike-through emphasis for Org.")

  (defface my-org-emphasis-strike-through
    '((((class color) (min-colors 88) (background light))
       :strike-through "#972500" :foreground "#505050")
      (((class color) (min-colors 88) (background dark))
       :strike-through "#ef8b50" :foreground "#a8a8a8"))
    "My strike-through emphasis for Org.")

  (setq org-emphasis-alist
        '(("*" my-org-emphasis-bold)
          ("/" my-org-emphasis-italic)
          ("_" my-org-emphasis-underline)
          ("=" org-verbatim verbatim)
          ("~" org-code verbatim)
          ("+" my-org-emphasis-strike-through))))

(use-package ob-core
  :after org
  :config
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
  (advice-add 'org-babel-execute-src-block :before 'my/org-babel-execute-src-block)
  (setq org-confirm-babel-evaluate nil))

(use-package ob-python
  :after ob-core
  :config
  (setq org-babel-python-command "/usr/bin/python3"))

(use-package org-capture
  :bind (("<f10>" . org-capture)
         (:map org-capture-mode-map
               ([remap evil-save-and-close] . org-capture-finalize)
               ([remap evil-save-modified-and-close] . org-capture-finalize)
               ([remap evil-quit] . org-capture-kill)))
  :config
  (setq org-capture-templates
        '(("i" "GTD Inbox"
           entry (file (lambda () (concat mobile-document "iCloud~com~appsonthemove~beorg/Documents/org/inbox.org")))
           "* %?\n%U\n" :time-prompt t :tree-type week)
          ;; ("I" "NC Inbox"
          ;;  plain (file+olp+datetree (lambda () (concat my-galaxy "/inbox/inbox.org")))
          ;;  "**** %?\n%U\n" :time-prompt t :tree-type week)
          ("w" "Work log"
           plain
           (file+olp+datetree (lambda () (concat my-galaxy "/logs/worklog.org")))
           (file "~/.emacs.d/template/tpl-worklog")
           :time-prompt t :tree-type week)
          ;; ("p" "Daily Plan"
          ;;  plain (file+olp+datetree (lambda () (concat my-galaxy "/inbox/plan.org")))
          ;;  "%?\n%U\n" :time-prompt t :tree-type week)
          ("r" "Reflection"
           plain
           (file+olp+datetree (lambda () (concat my-galaxy "/logs/reflection.org")))
           (file "~/.emacs.d/template/tpl-daily-reflection")
           :time-prompt t :tree-type week)
          ;; ("a" "Anki Deck")
          ;; ("ae" "Deck: English"
          ;;  entry (file (lambda ()
          ;;                (concat my-galaxy "/anki/anki_english.org")))
          ;;  "* %?\n" :jump-to-captured t)
          ;; ("ac" "Deck: Civil Engineering"
          ;;  entry (file (lambda ()
          ;;                (concat my-galaxy "/anki/anki_engineering.org")))
          ;;  "* %?\n" :jump-to-captured t)
          ;; ("s" "Code snippets"
          ;;  entry (file (lambda ()
          ;;                (concat my-galaxy "/scripts/snippets.org")))
          ;;  "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
          ("m" "Movie"
           entry (file+headline (lambda () (concat my-galaxy "/logs/watchlist.org")) "Watching Lists")
           "* %?
  :PROPERTIES:
  :GENRE: %^{Film genre|Action|Adventure|Comedy|Drama|Fantasy|Horror|Musicals|Mystery|Romance|Science fiction|Sports|Thriller}
  :COUNTRY:
  :SCORE:
  :PLOT: %^{PLOT}
  :END:"))))

(use-package org-attach
  :commands org-attach
  :init
  (add-to-list 'display-buffer-alist
               '("\\*Org Attach\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.5)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))
  :config
  (setq org-attach-id-dir (expand-file-name "attach" my-galaxy))
  (setq org-attach-id-to-path-function-list
        '(org-attach-id-ts-folder-format
          org-attach-id-uuid-folder-format)))

;;;###autoload
(defun my/org-attach-visit-headline-from-dired ()
    "Go to the headline corresponding to this org-attach directory."
    (interactive)
    (let* ((id-parts (last (split-string default-directory "/" t) 2))
           (id (apply #'concat id-parts)))
      (let ((m (org-id-find id 'marker)))
        (unless m (user-error "Cannot find entry with ID \"%s\"" id))
        (pop-to-buffer (marker-buffer m))
        (goto-char m)
        (move-marker m nil)
        (org-fold-show-context))))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-'") #'my/org-attach-visit-headline-from-dired))

(use-package org-habit
  :after org
  :config
  (add-to-list 'org-modules 'org-habit t))

(use-package org-id
  :after org
  :config
  (setq org-id-locations-file (expand-file-name "cache/.org-id-locations" user-emacs-directory))
  (setq org-id-method 'ts)
  (setq org-id-link-to-org-use-id 'create-if-interactive))

;;;###autoload
(defun my/copy-idlink ()
  "Copy idlink to clipboard."
  (interactive)
  (when (eq major-mode 'org-agenda-mode) ;switch to orgmode
    ;; (org-agenda-show)
    (org-agenda-goto))
  (when (eq major-mode 'org-mode) ; do this only in org-mode buffers
    (let* ((mytmphead (nth 4 (org-heading-components)))
           (mytmpid (funcall 'org-id-get-create))
           (mytmplink (format "- [ ] [[id:%s][%s]]" mytmpid mytmphead)))
      (kill-new mytmplink)
      (message "Copied %s to killring (clipboard)" mytmplink))))
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "<f8>") 'my/copy-idlink))

(use-package org-src
  :after org
  :config
  (setq org-src-window-setup 'current-window)
  (setq org-src-ask-before-returning-to-edit-buffer nil))

(use-package org-refile
  :commands org-refile
  :config
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-active-region-within-subtree t))

(use-package org-clock
  :commands org-clocking-p
  :hook ((org-clock-in . my/afplay-clock-in)
         (org-clock-out . my/afplay-clock-out))
  :config
  (defun my/afplay-clock-in ()
    (async-shell-command "afplay /System/Library/Sounds/Ping.aiff"))
  (defun my/afplay-clock-out ()
    (async-shell-command "afplay /System/Library/Sounds/Basso.aiff"))

  (org-clock-persistence-insinuate)
  (setq org-clock-persist-file (expand-file-name "cache/org-clock-save.el" user-emacs-directory))
  (setq org-clock-history-length 23)
  (setq org-clock-in-resume t)
  (setq org-clock-into-drawer "LOGCLOCK")
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-clock-out-when-done t)
  (setq org-clock-persist t)
  (setq org-clock-clocktable-default-properties '(:maxlevel 5 :link t :tags t))
  (setq org-clock-persist-query-resume nil)
  (setq org-clock-report-include-clocking-task t))

(use-package org-indent
  :hook (org-mode . org-indent-mode))

(use-package ox-html
  :after ox
  :config
  (setq org-export-global-macros
        '(("timestamp" . "@@html:<span class=\"timestamp\">[$1]</span>@@")))
  (setq org-html-preamble t)
  (setq org-html-preamble-format
      '(("en" "<a href=\"/index.html\" class=\"button\">Home</a>
               <a href=\"/posts/index.html\" class=\"button\">Posts</a>
               <a href=\"/about.html\" class=\"button\">About</a>
               <hr>")))

  (setq org-html-postamble t)

  (setq org-html-postamble-format
        '(("en" "<hr><div class=\"info\"> <span class=\"created\">Created with %c on MacOS</span>
 <span class=\"updated\">Updated: %d</span> </div>")))

  (setq org-html-head-include-default-style nil)

  (setq org-html-head
        "<link rel=\"stylesheet\" type=\"text/css\" href=\"/css/style.css\" />
         <script src=\"js/copy.js\"></script> "))

(use-package ox-publish
  :after ox
  :config
  ;; https://git.sr.ht/~taingram/taingram.org/tree/master/item/publish.el
  (defun taingram--sitemap-dated-entry-format (entry style project)
    "Sitemap PROJECT ENTRY STYLE format that includes date."
    (let ((filename (org-publish-find-title entry project)))
      (if (= (length filename) 0)
          (format "*%s*" entry)
        (format "{{{timestamp(%s)}}}   [[file:%s][%s]]"
                (format-time-string "%Y-%m-%d"
                                    (org-publish-find-date entry project))
                entry
                filename))))

  (defvar my/publish-directory "~/Blogs/")

  (setq org-publish-project-alist
        `(("site"
           :base-directory ,website-directory
           :base-extension "org"
           :recursive nil
           :publishing-directory ,my/publish-directory
           :publishing-function org-html-publish-to-html)

          ("posts"
           :base-directory ,(expand-file-name "posts" website-directory)
           :base-extension "org"
           :publishing-directory ,(expand-file-name "posts" my/publish-directory)
           :publishing-function org-html-publish-to-html
           :with-author t
           :auto-sitemap t
           :sitemap-filename "index.org"
           :sitemap-title "posts"
           :sitemap-sort-files anti-chronologically
           :sitemap-format-entry taingram--sitemap-dated-entry-format)

          ("static"
           :base-directory ,website-directory
           :base-extension "css\\|js\\|txt\\|jpg\\|gif\\|png"
           :recursive t
           :publishing-directory  ,my/publish-directory
           :publishing-function org-publish-attachment)

          ("personal-website" :components ("site" "posts" "static")))))

(provide 'init-org)
;;; init-org.el ends here.
