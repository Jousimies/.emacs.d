;; init-git.el --- Version manager. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package magit
  :commands (magit magit-status magit-submodule-add)
  :bind ("C-x g" . magit)
  :config
  (setq magit-git-executable "/usr/bin/git")
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-module-sections-hook 'magit-insert-modules-overview)
  (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpulled-from-pushremote)
  (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpushed-to-upstream)
  (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpushed-to-pushremote))

(use-package transient
  :defer t
  :config
  (setq transient-levels-file (expand-file-name "cache/transient/levels.el" user-emacs-directory))
  (setq transient-values-file (expand-file-name "cache/transient/values.el" user-emacs-directory))
  (setq transient-history-file (expand-file-name "cache/transient/history.el" user-emacs-directory)))

(defvar my/vc-type '("feat: feature (A new feature)"
                     "fix: bug fix (A bug fix)"
                     "docs: docs (Changes to documentation)"
                     "style: style (Formatting, missing semi colons, etc; no code change)"
                     "refactor: refactor (Refactoring production code)"
                     "test: tests (Adding tests, refactoring test; no production code change)"
                     "chore: chore (Updating build tasks, package manager configs, etc; no production code change)")
  "Git commit guildlines.")

(cl-defun jf/git-commit-mode-hook (&key (splitter ":") (padding ""))
  "If the first line is empty, prompt for commit type and insert it.

Add PADDING between inserted commit type and start of title.  For
the `completing-read' show the whole message.  But use the
SPLITTER to determine the prefix to include."
  (when (and (eq major-mode 'text-mode)
             (string= (buffer-name) "COMMIT_EDITMSG")
             ;; Is the first line empty?
             (save-excursion
               (goto-char (point-min))
               (beginning-of-line-text)
               (looking-at-p "^$")))
    (let ((commit-type (completing-read "Commit title prefix: "
                                        my/vc-type nil t)))
      (goto-char (point-min))
      (insert (car (s-split splitter commit-type)) "():")
      (backward-char 2))))

(add-hook 'find-file-hook 'jf/git-commit-mode-hook)

(use-package forge
  :after magit
  :config
  (setq forge-database-file (expand-file-name "cache/forge-database.sqlite" user-emacs-directory)))

(use-package git-timemachine
  :general (my/space-leader-def
             "mt" '(git-timemachine :wk "Timemachine")))

(use-package browse-at-remote
  :bind ("M-<f4>" . browse-at-remote))

(provide 'init-git)
;;; init-git.el ends here.
