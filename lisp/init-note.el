;; init-note.el.el --- Note taking. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package denote
  :bind (("C-c n s" . denote-signature)
         ("C-c n i" . denote-insert-link)
         ("C-c n r" . denote-rename-file-using-front-matter)
         ("C-c n k" . denote-keywords-add)
         ("C-c n K" . denote-keywords-remove))
  :commands (denote denote-signature denote-subdirectory denote-rename-file-using-front-matter
                    denote-keywords-prompt
                    denote-rename-file
                    denote-link-or-create)
  :hook (dired-mode . denote-dired-mode-in-directories)
  :config
  (setq denote-directory (expand-file-name "denote" my-galaxy))
  (setq denote-dired-directories (list denote-directory
                                       (thread-last denote-directory (expand-file-name "books"))
                                       (thread-last denote-directory (expand-file-name "outline"))
                                       (thread-last denote-directory (expand-file-name "literature"))
                                       (thread-last denote-directory (expand-file-name "term"))
                                       (thread-last denote-directory (expand-file-name "references")))))

(use-package denote-org-dblock
  :commands denote-org-dblock-insert-backlinks denote-org-dblock-insert-links)

(defvar prot-dired--limit-hist '()
  "Minibuffer history for `prot-dired-limit-regexp'.")

(defun my/dired-denote-signature-get ()
  (let* ((file (dired-get-filename))
         (signature (denote-retrieve-filename-signature file)))
    (concat "==" signature )))

(defun prot-dired-limit-regexp (regexp omit)
  "Limit Dired to keep files matching REGEXP, default search with Signature.

With optional OMIT argument as a prefix (\\[universal-argument]),
exclude files matching REGEXP.

Restore the buffer with \\<dired-mode-map>`\\[revert-buffer]'."
  (interactive
   (list
    (read-regexp
     (concat "Files "
             (when current-prefix-arg
               (propertize "NOT " 'face 'warning))
             "matching PATTERN" (format " (default: %s)" (my/dired-denote-signature-get)) ": ")
     (my/dired-denote-signature-get)
     nil)
    current-prefix-arg))
  (dired-mark-files-regexp regexp)
  (unless omit (dired-toggle-marks))
  (dired-do-kill-lines))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-;") 'prot-dired-limit-regexp))

(defun my/org-capture-safari-literature ()
  "Create an `literature' denote entry from Safari page."
  (interactive)
  (let* ((url (car (grab-mac-link-safari-1)))
         (title (cadr (grab-mac-link-safari-1)))
         (keywords (denote-keywords-prompt))
         (ID (format-time-string "%Y%m%dT%H%M%S"))
         (new-title (concat ID "--" title))
         (file-path (concat my/web_archive title ".html"))
         (file-new-path (concat my/web_archive new-title ".html")))
    (if (not (file-exists-p file-path))
        (message "Please save webpage first!!!")
      ;; (x-popup-dialog (selected-frame) '("Please save web page first!!!" ("OK" . :yes))
      (rename-file file-path file-new-path)
      (denote title
              keywords
              'org
              (expand-file-name "literature" (denote-directory))
              nil)
      (save-excursion
        (goto-char (point-max))
        (insert "* ")
        (org-insert-link nil file-new-path title)
        (org-set-property "URL" url)
        (org-set-tags "Reference")
        (my/auto-change-file-paths)))))

(global-set-key (kbd "M-<f10>") 'my/org-capture-safari-literature)

(cl-defun my/denote-subdirectory (subdirectory)
  (denote
   (denote-title-prompt)
   (denote-keywords-prompt)
   'org
   (expand-file-name subdirectory (denote-directory))))

;;;###autoload
(defun my/denote-term ()
  (interactive)
  (my/denote-subdirectory "term"))

;;;###autoload
(defun my/denote-book ()
  (interactive)
  (my/denote-subdirectory "books"))

;;;###autoload
(defun my/denote-outline ()
  (interactive)
  (my/denote-subdirectory "outline"))

;;;###autoload
(defun my/denote-reference-heading ()
  (interactive)
  (goto-char (point-max))
  (insert "\n* References\n"))

;;;###autoload
(defun my/denote-signature-from-filename ()
  (interactive)
  (let* ((mode (buffer-local-value 'major-mode (current-buffer)))
         (file (if (eq mode 'org-mode) (buffer-file-name) (dired-get-filename)))
         (signature (denote-retrieve-filename-signature file)))
    (if signature
        (kill-new signature))))

(advice-add 'denote-signature :before #'my/denote-signature-from-filename)

(defun my/denote-signature-or-subdirectory (arg)
  (interactive "P")
  (if arg
      (one-key-menu-new)
    (denote-signature)))

(defun my/denote-signature-buffer ()
    (interactive)
    (switch-to-buffer "*denote-signatures*")
    (read-only-mode -1)
    (erase-buffer)
    (insert
     (shell-command-to-string
      "ls -l | awk /==/ | sed  's/--/=@/3' | sort -t '=' -Vk 3,3 | sed 's/=@/--/' "))
    (dired-clear-alist)
    (dired-virtual denote-directory)
    (denote-dired-mode)
    (auto-revert-mode -1))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-,") 'my/denote-signature-buffer))

(use-package denote-menu
  :commands denote-menu-list-notes)

(use-package consult-notes
  :bind ("C-c n n" . consult-notes)
  :config
  (setq consult-notes-file-dir-sources
        `(("Articles"  ?a  ,(concat my-galaxy "/blogs_source/posts"))
          ("Denote Notes"  ?d ,(expand-file-name "denote" my-galaxy))
          ("Terminology"  ?t ,(expand-file-name "denote/term" my-galaxy))
          ("Book Reading"  ?b ,(expand-file-name "denote/books" my-galaxy))
          ("Outline"  ?o ,(expand-file-name "denote/outline" my-galaxy))
          ("Meet"  ?m ,(expand-file-name "meeting" my-galaxy))
          ("References"  ?r ,(expand-file-name "denote/references" my-galaxy))
          ("Literature"  ?l ,(expand-file-name "denote/literature" my-galaxy)))))

(defun my/new-article (article)
    (interactive "sTitle: ")
    (let ((filename (format "%s" article))
          (ext ".org"))
      (find-file (concat website-directory "posts/" filename ext))
      (insert "#+TITLE: " article "\n")
      (tempel-insert 'blog)))
(global-set-key (kbd "C-c n a") 'my/new-article)

(defun my/new-meeting (meet)
  (interactive "sTitle: ")
  (let ((filename (format "%s-%s" (format-time-string "%Y%m%d") meet))
        (ext ".org"))
    (find-file (concat my-galaxy "/meeting/" filename ext))
    (insert "#+TITLE: " meet "\n")
    (tempel-insert 'meeting)))

(global-set-key (kbd "C-c n m") 'my/new-meeting)

(use-package org-change
  :commands org-change-add)

(provide 'init-note)
;;; init-note.el ends here.
