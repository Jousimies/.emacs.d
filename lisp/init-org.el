;; -*- lexical-binding: t; -*-

;; Preload Org incrementally so its first interactive use stays responsive.
(my/idle-loader-add '(0.7 . (require 'org nil t))
                    '(require 'org-id nil t)
                    '(require 'org-agenda nil t)
                    '(require 'org-capture nil t)
                    '(require 'org-clock nil t)
                    '(require 'org-attach nil t)
                    '(require 'org-refile nil t)
                    '(require 'org-goto nil t)
                    '(require 'org-archive nil t))

(with-eval-after-load 'org
  (setq org-modules nil
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
  (defcustom my/org-babel-trusted-languages '(emacs-lisp shell python C)
    "Babel languages allowed without confirmation in trusted local files.
Other languages remain available on demand, but require confirmation before
their backend is loaded and their code is evaluated."
    :type '(repeat symbol)
    :group 'org-babel)

  (defun my/org-babel-language-symbol (language)
    "Return the Babel backend symbol corresponding to LANGUAGE."
    (pcase (downcase language)
      ((or "c" "cpp" "c++") 'C)
      ((or "sh" "bash" "zsh" "fish" "shell") 'shell)
      ("r" 'R)
      (_ (intern language))))

  (defun my/org-babel-trusted-context-p (info)
    "Return non-nil when Babel block INFO is safe for no-prompt execution."
    (let* ((language (my/org-babel-language-symbol (car info)))
           (params (nth 2 info))
           (dir (cdr (assq :dir params))))
      (and (memq language my/org-babel-trusted-languages)
           buffer-file-name
           (not (file-remote-p buffer-file-name))
           (not (file-remote-p default-directory))
           (or (not (stringp dir))
               (not (file-remote-p (expand-file-name dir)))))))

  (defun my/org-babel-confirm-evaluate-p (language _body)
    "Request confirmation unless LANGUAGE is trusted in this local buffer."
    (not (and (memq (my/org-babel-language-symbol language)
                    my/org-babel-trusted-languages)
              buffer-file-name
              (not (file-remote-p buffer-file-name))
              (not (file-remote-p default-directory)))))

  (defun my/org-babel-load-language-after-confirmation (orig-fun info)
    "Call ORIG-FUN for INFO, then load its backend only when approved."
    (let ((org-confirm-babel-evaluate
           (not (my/org-babel-trusted-context-p info))))
      (when (funcall orig-fun info)
        (let* ((language (my/org-babel-language-symbol (car info)))
               (feature (intern (format "ob-%s" language))))
          (unless (require feature nil t)
            (user-error "No Org Babel backend found for %s" (car info)))
          (cl-pushnew (cons language t) org-babel-load-languages
                      :key #'car)
          t))))

  (setq org-confirm-babel-evaluate #'my/org-babel-confirm-evaluate-p)
  (unless (advice-member-p #'my/org-babel-load-language-after-confirmation
                           'org-babel-confirm-evaluate)
    (advice-add 'org-babel-confirm-evaluate :around
                #'my/org-babel-load-language-after-confirmation)))

;; org-refile
(with-eval-after-load 'org-refile
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9))
	org-refile-use-outline-path t
	org-outline-path-complete-in-steps nil
	org-refile-allow-creating-parent-nodes 'confirm
	org-refile-use-outline-path 'file
	org-refile-active-region-within-subtree t))

;; org-attach
(with-eval-after-load 'org-attach
  (defun org-attach-save-file-list-to-property (dir)
    "Save list of attachments to ORG_ATTACH_FILES property."
    (when-let* ((files (org-attach-file-list dir)))
      (org-set-property "ORG_ATTACH_FILES" (mapconcat #'identity files ", "))))
  (add-hook 'org-attach-after-change-hook #'org-attach-save-file-list-to-property)

  (defun update-org-attach-property ()
    "Manually update the ORG_ATTACH_FILES property for the current Org entry."
    (interactive)
    (let* ((dir (org-attach-dir t))
           (files (org-attach-file-list dir)))
      (when (and dir files)
	(org-with-wide-buffer
	 (org-set-property "ORG_ATTACH_FILES" (mapconcat #'identity files ", ")))
	(message "ORG_ATTACH_FILES property updated."))))

  (setq org-attach-expert t
	org-attach-id-dir (expand-file-name "attach" my-galaxy)
	org-attach-id-to-path-function-list '(org-attach-id-ts-folder-format
					      org-attach-id-uuid-folder-format)))

;; org-id
(with-eval-after-load 'org-id
  (defun update-org-ids-in-directory (directory)
    "Update Org IDs in all Org files in DIRECTORY."
    (interactive "DEnter directory: ")
    (require 'org-id)
    (when (file-directory-p directory)
      (let ((org-files (directory-files-recursively directory "\\.org\\'")))
	(org-id-update-id-locations org-files t)
	(message "Updated Org IDs in %d files." (length org-files))))
    (unless (file-directory-p directory)
      (message "Not a valid directory: %s" directory)))

  (setq org-id-method 'ts
	org-id-locations-file (expand-file-name ".org-id-locations" cache-directory)
	org-id-link-to-org-use-id 'create-if-interactive))

;; org-src
(with-eval-after-load 'org-src
  (setq org-src-window-setup 'current-window
	org-src-ask-before-returning-to-edit-buffer nil))

;; org-goto
(with-eval-after-load 'org-goto
  (setq org-goto-interface 'outline-path-completion))

;; org-clock
(with-eval-after-load 'org-clock
  (setq org-clock-persist-file (expand-file-name "org-clock-save.el" cache-directory)
	org-clock-history-length 23
	org-clock-in-resume t
	org-clock-into-drawer "LOGCLOCK"
	;; org-clock-idle-time 15
	org-clock-out-remove-zero-time-clocks t
	org-clock-out-when-done t
	org-clock-persist 'history
	org-clock-clocktable-default-properties '(:maxlevel 5 :link t :tags t)
	org-clock-persist-query-resume nil
	org-clock-report-include-clocking-task t)
  ;; `org-todo' already calls `org-clock-out-if-current' before running the
  ;; state-change hook.  With this option enabled it stops only the clocked
  ;; entry, and only when that entry enters an Org done state.
  )

;; org-archive
(with-eval-after-load 'org-archive
  (defun my/org-gtd-dynamic-archive-location (orig-fun &rest args)
    (let* ((closed (org-entry-get nil "CLOSED"))
           (year (if (and closed (string-match "\\([0-9]\\{4\\}\\)" closed))
                     (match-string 1 closed)
                   (format-time-string "%Y")))
           (org-archive-location
            (concat (expand-file-name (format "gtd_archive_%s" year) org-gtd-directory)
                    "::datetree/")))
      (apply orig-fun args)))

  (advice-add 'org-archive-subtree :around #'my/org-gtd-dynamic-archive-location))


(provide 'init-org)
