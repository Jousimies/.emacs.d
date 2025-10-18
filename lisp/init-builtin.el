;;; init-builtin.el --- Emacs better builtin packages  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Duan Ning

;; Author: Duan Ning <duan_n@outlook.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(add-hook 'after-init-hook #'column-number-mode)
(add-hook 'after-init-hook #'size-indication-mode)
(add-hook 'org-mode-hook #'visual-line-mode)
(add-hook 'eww-mode #'visual-line-mode)
(add-hook 'after-init-hook #'global-so-long-mode)
(add-hook 'after-init-hook #'global-prettify-symbols-mode)
(add-hook 'after-init-hook #'global-word-wrap-whitespace-mode)
(add-hook 'after-init-hook #'midnight-mode)
(add-hook 'minibuffer-mode-hook #'minibuffer-electric-default-mode)
(add-hook 'after-init-hook #'auto-insert-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'org-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'find-file-hook 'show-paren-mode)
(add-hook 'after-init-hook 'winner-mode)
(add-hook 'after-init-hook #'windmove-mode)
(add-hook 'after-init-hook 'pixel-scroll-mode)
(add-hook 'after-init-hook #'desktop-save-mode)
(add-hook 'after-init-hook #'repeat-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'package-menu-mode #'hl-line-mode)
(add-hook 'calendar-today-visible-hook #'calendar-mark-today)
(add-hook 'diary-mode-hook #'appt-activate)
(add-hook 'diary-list-entries-hook #'diary-sort-entries)
(add-hook 'diary-mode-hook #'goto-address-mode)
(add-hook 'minibuffer-mode-hook #'minibuffer-electric-default-mode)
(add-hook 'minibuffer-mode-hook #'cursor-intangible-mode)
(add-hook 'completion-list-mode-hook (lambda ()
				       (setq-local truncate-lines t)))
(add-hook 'minibuffer-setup-hook (lambda ()
				   (setq-local truncate-lines t)))

(mouse-avoidance-mode 'jump)

(setopt mark-ring-max 128
        kill-do-not-save-duplicates t
        kill-ring-max (* kill-ring-max 2)
        async-shell-command-display-buffer nil)

;; auto-save
(setopt auto-save-default nil
        auto-save-visited-interval 1
        save-silently t
        large-file-warning-threshold nil
        confirm-kill-processes nil
        confirm-kill-emacs nil
        make-backup-files nil
        view-read-only t
        kill-read-only-ok t
	isearch-lazy-count t
	help-window-select 'other
	help-window-keep-selected t
	ad-redefinition-action 'accept
	truncate-string-ellipsis "…"
	multisession-directory (expand-file-name "multisession" cache-directory)
	auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" cache-directory))

;; (setq backup-directory-alist '(("." . "~/.emacs.d/cache/backups")))
(add-hook 'after-init-hook #'auto-save-visited-mode)

(defun auto-save-delete-trailing-whitespace-except-current-line ()
  (interactive)
  (let ((begin (line-beginning-position))
        (end (point))
        (buffername (buffer-name (buffer-base-buffer))))
    (when (not (or (string-prefix-p "inbox" buffername)
                   (string-match-p "^[0-9]" buffername)))
      (save-excursion
        (when (< (point-min) begin)
          (save-restriction
            (narrow-to-region (point-min) (1- begin))
            (delete-trailing-whitespace)))
        (when (> (point-max) end)
          (save-restriction
            (narrow-to-region end (point-max))
            (delete-trailing-whitespace)))))))
(add-hook 'before-save-hook #'auto-save-delete-trailing-whitespace-except-current-line)

(defun switch-to-message ()
  "Quick switch to `*Message*' buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(setopt message-kill-buffer-on-exit t
        message-kill-buffer-query nil
        message-sendmail-envelope-from 'header
        message-sendmail-extra-arguments '("-a" "outlook"))

(global-set-key (kbd "M-g m") #'switch-to-message)
(global-set-key (kbd "M-g s") #'scratch-buffer)

;; (when is-fullscreen
;;   (add-hook 'after-init-hook 'display-battery-mode)
;;   (display-time-mode))

(when display-time-mode
  (setopt display-time-default-load-average nil)
  (setopt display-time-mail-string "")
  (setopt display-time-format "%H:%M")
  (setopt display-time-string-forms
          '((propertize
             (format-time-string
              (or display-time-format
                  (if display-time-24hr-format "%H:%M" "%-I:%M%p"))
              now)
             'face 'display-time-date-and-time
             'help-echo (format-time-string "%a %b %e, %Y" now))
            " ")))

(setopt prettify-symbols-alist '(("lambda" . ?λ)
				 ("function" . ?𝑓)))

;;Bookmark
(setq bookmark-default-file (expand-file-name "bookmarks" cache-directory))

;; auto-insert-mode
(with-eval-after-load 'auto-insert
  (add-to-list 'auto-insert-alist
               '(("\\.el\\'" . "Emacs Lisp header")
                 "Short description: "
                 ";;; " (file-name-nondirectory (buffer-file-name)) " --- " str
                 (make-string (max 2 (- 80 (current-column) 27)) ?\s)
                 "-*- lexical-binding: t; -*-" '(setq lexical-binding t)
                 "

;; Copyright (C) " (format-time-string "%Y") "  "
                 (getenv "ORGANIZATION") | (progn user-full-name) "

;; Author: " (user-full-name)
                 '(if (search-backward "&" (line-beginning-position) t)
                      (replace-match (capitalize (user-login-name)) t t))
                 '(end-of-line 1) " <" (progn user-mail-address) ">
"
                 ;; Keywords and completing-read with a require-match don't give me a way to break out
                 ;; ;; Keywords: "
                 ;;  '(require 'finder)
                 ;;  ;;'(setq v1 (apply 'vector (mapcar 'car finder-known-keywords)))
                 ;;  '(setq v1 (mapcar (lambda (x) (list (symbol-name (car x))))
                 ;;       finder-known-keywords)
                 ;;  v2 (mapconcat (lambda (x) (format "%12s:  %s" (car x) (cdr x)))
                 ;;     finder-known-keywords
                 ;;     "\n"))
                 ;;  ((let ((minibuffer-help-form v2))
                 ;;     (completing-read "Keyword, C-h: " v1 nil t))
                 ;;     str ", ")
                 ;; & -2
                 "

\;; This program is free software; you can redistribute it and/or modify
\;; it under the terms of the GNU General Public License as published by
\;; the Free Software Foundation, either version 3 of the License, or
\;; (at your option) any later version.

\;; This program is distributed in the hope that it will be useful,
\;; but WITHOUT ANY WARRANTY; without even the implied warranty of
\;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
\;; GNU General Public License for more details.

\;; You should have received a copy of the GNU General Public License
\;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

\;;; Commentary:

\;; " _ "

\;;; Code:



\(provide '"
                 (file-name-base (buffer-file-name))
                 ")
\;;; " (file-name-nondirectory (buffer-file-name)) " ends here\n")))


(with-eval-after-load 'display-line-numbers
  (setopt display-line-numbers-widen t
	  display-line-numbers-type 'relative))

(with-eval-after-load 'display-fill-column-indicator
  (face-spec-set 'fill-column-indicator
               '((default :height 0.1))
               'face-override-spec)
  (setq-default fill-column 90))

;; (define-fringe-bitmap 'right-curly-arrow  [])
;; (define-fringe-bitmap 'left-curly-arrow  [])
;; https://xenodium.com/toggling-emacs-continuation-fringe-indicator/
;; (setq-default fringe-indicator-alist
;;               (delq (assq 'continuation fringe-indicator-alist)
;; 		    fringe-indicator-alist))

(defun toggle-continuation-fringe-indicator ()
  (interactive)
  (setq-default
   fringe-indicator-alist
   (if (assq 'continuation fringe-indicator-alist)
       (delq (assq 'continuation fringe-indicator-alist) fringe-indicator-alist)
     (cons '(continuation right-curly-arrow left-curly-arrow) fringe-indicator-alist))))

(with-eval-after-load 'paren
  (setopt show-paren-style 'parenthesis
	  show-paren-context-when-offscreen 'overlay
	  show-paren-highlight-openparen t
	  show-paren-when-point-inside-paren t
	  show-paren-when-point-in-periphery t))

(with-eval-after-load 'frame
  (setopt window-divider-default-bottom-width 1
	  window-divider-default-places 'bottom-only))

(global-set-key (kbd "M-g u") #'winner-undo)
(global-set-key (kbd "M-g r") #'winner-redo)
(with-eval-after-load 'winner
  (setopt winner-dont-bind-my-keys t
	  winner-boring-buffers '("*Completions*"
				  "*Compile-Log*"
				  "*inferior-lisp*"
				  "*Fuzzy Completions*"
				  "*Apropos*"
				  "*Help*"
				  "*cvs*"
				  "*Buffer List*"
				  "*Ibuffer*"
				  "*esh command on file*")))

(add-to-list 'display-buffer-alist '("\\*Outline"
                                     (display-buffer-in-side-window)
                                     (side . right)
                                     (window-width . 0.5)))

(global-set-key (kbd "M-g h") #'windmove-left)
(global-set-key (kbd "M-g l") #'windmove-right)
(global-set-key (kbd "M-g k") #'windmove-up)
(global-set-key (kbd "M-g j") #'windmove-down)

(with-eval-after-load 'window
  (setopt switch-to-buffer-in-dedicated-window 'pop
	  switch-to-buffer-obey-display-actions t))

;; desktop
;; desktop 和 persp 合用会导致 Unprintable entity 问题
(with-eval-after-load 'desktop
  (setopt desktop-path `(,cache-directory)))

(defun my/scroll-other-windown-down ()
  "Scroll other window down."
  (interactive)
  (scroll-other-window-down 2))

(global-set-key (kbd "M-p") 'my/scroll-other-windown-down)

(defun my/scroll-other-windown ()
  "Scroll other window up."
  (interactive)
  (scroll-other-window 2))

(global-set-key (kbd "M-n") 'my/scroll-other-windown)

;; url-history
(setopt url-configuration-directory (expand-file-name "url" cache-directory))
(setopt url-history-file (expand-file-name "history" url-configuration-directory))
(setopt url-cookie-file (expand-file-name "cookies" url-configuration-directory))

;; xref
;; Prefer ripgrep, then ugrep, and fall back to regular grep.
(setopt xref-search-program (cond
			     ((or (executable-find "ripgrep")
				  (executable-find "rg"))
			      'ripgrep)
			     ((executable-find "ugrep")
			      'ugrep)
			     (t
			      'grep)))

;;;; Repeatable key chords (repeat-mode)
(with-eval-after-load 'repeat
  (setopt repeat-on-final-keystroke t
	  repeat-exit-timeout 5
	  repeat-exit-key "<escape>"
	  repeat-keep-prefix nil
	  repeat-check-key t
	  set-mark-command-repeat-pop t))

(with-eval-after-load 'hl-line
  (setopt hl-line-sticky-flag nil))

(global-set-key (kbd "C-x c") #'calendar)
(with-eval-after-load 'calendar
  (setopt calendar-view-diary-initially-flag t
	  calendar-mark-diary-entries-flag t
	  calendar-date-style 'iso
	  calendar-date-display-form calendar-iso-date-display-form
	  diary-date-forms diary-iso-date-forms
	  calendar-time-display-form '(24-hours ":" minutes
						(when time-zone
						  (format "(%s)" time-zone)))))

;; Tramp
(with-eval-after-load 'tramp
  (setq tramp-persistency-file-name (expand-file-name "tramp" cache-directory)))

(global-set-key (kbd "C-<f5>") #'eshell)
(with-eval-after-load 'eshell
  (setopt eshell-directory-name (expand-file-name "eshell" cache-directory)))

;; ispell and flyspell
;; (use-package flyspell
;;   :hook (text-mode . flyspell-mode)
;;   :custom
;;   (flyspell-issue-message-flag nil)
;;   (ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))

(with-eval-after-load 'appt
  (setopt appt-display-diary nil
	  appt-disp-window-function #'appt-disp-window
	  appt-display-mode-line t
	  appt-display-interval 3
	  appt-audible nil
	  appt-warning-time-regexp "appt \\([0-9]+\\)"
	  appt-message-warning-time 6))

(with-eval-after-load 'diary-lib
  (setopt diary-display-function #'diary-fancy-display
	  diary-header-line-format nil
	  diary-list-include-blanks nil
	  diary-abbreviated-year-flag nil
	  diary-number-of-entries 7
	  diary-comment-start ");;"
	  diary-comment-end ""
	  diary-nonmarking-symbol "!"
	  diary-file (expand-file-name "logs/diary.org" my-galaxy)))


(add-hook 'after-init-hook #'global-auto-revert-mode)

(with-eval-after-load 'register
  (setopt register-preview-delay 0)
  (set-register ?a (cons 'file (concat icloud "iCloud~com~appsonthemove~beorg/Documents/org/gtd_archive_" (format-time-string "%Y"))))
  (set-register ?f (cons 'file (concat icloud "iCloud~com~appsonthemove~beorg/Documents/org/flash_thoughts_" (format-time-string "%Y"))))
  (set-register ?t (cons 'file (expand-file-name "iCloud~com~appsonthemove~beorg/Documents/org/org-gtd-tasks.org" icloud)))
  (set-register ?r (cons 'file (expand-file-name (format-time-string "logs/weekly_review_%Y.org") my-galaxy)))
  (set-register ?l (cons 'file (expand-file-name (format-time-string "logs/work_log_%Y.org") my-galaxy))))

(defun my/open-recentf ()
  (interactive)
  (unless recentf-mode (recentf-mode 1))
  (consult-recent-file))

(with-eval-after-load 'recentf
  (setopt recentf-save-file (expand-file-name "recentf" cache-directory)
	  recentf-auto-cleanup 300
	  recentf-max-saved-items 1000
	  recentf-exclude '("~/.telega")))

(add-hook 'after-init-hook #'savehist-mode)
(with-eval-after-load 'savehist
  (setopt savehist-file (expand-file-name "history" cache-directory)
	  history-length 1000
          savehist-additional-variables '(kill-ring
                                          search-ring
                                          regexp-search-ring)
          history-delete-duplicates t))

(setopt save-place-file (expand-file-name "places" cache-directory))
(add-hook 'after-init-hook #'save-place-mode)

(use-package hippie-exp
  :bind ([remap dabbrev-expand] . hippie-expand)
  :custom
  (hippie-expand-try-functions-list '(try-complete-file-name-partially
                                      try-complete-file-name
                                      try-expand-all-abbrevs
                                      try-expand-dabbrev
                                      try-expand-dabbrev-all-buffers
                                      try-expand-dabbrev-from-kill
                                      try-complete-lisp-symbol-partially
                                      try-complete-lisp-symbol)))

(add-hook 'after-init-hook #'electric-pair-mode)
;; (add-hook 'after-init-hook #'electric-quote-mode)
(add-hook 'after-init-hook #'electric-indent-mode)
(add-hook 'after-init-hook #'delete-selection-mode)

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :custom
  (ibuffer-default-sorting-mode 'major-mode))

(global-set-key (kbd "M-g p") 'previous-buffer)
(global-set-key (kbd "M-g n") 'next-buffer)

(setopt enable-recursive-minibuffers t)
(setopt read-minibuffer-restore-windows nil)
(setopt tab-always-indent 'complete
	tab-first-completion 'word-or-paren-or-punct
	completions-detailed t
        completions-format 'one-column
        completion-auto-select t
        completion-ignore-case t
        minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)
        completion-show-inline-help nil
        completions-max-height 50
        completion-show-help nil
        completion-auto-wrap nil
        completions-header-format (propertize "%s candidates:\n" 'face 'font-lock-comment-face)
        completions-highlight-face 'completions-highlight)

(keymap-set minibuffer-mode-map "C-r" #'minibuffer-complete-history)

(use-package which-key
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 0.1))

;; dired
(use-package dired
  :if (and IS-MAC
	   (executable-find "gls"))
  :custom
  (dired-use-ls-dired nil)
  (insert-directory-program "gls")
  (dired-listing-switches
   "-l --almost-all --human-readable --group-directories-first --no-group"))

(use-package dired
  :bind (:map dired-mode-map
	      ("C-'" . my/org-attach-visit-headline-from-dired))
  :hook (dired-mode . (lambda ()
			(setq-local truncate-lines t)))
  :custom
  (dired-dwim-target t)
  (dired-auto-revert-buffer #'dired-buffer-stale-p)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-auto-revert-buffer t)
  (dired-filename-display-length 'window))

(use-package dired-x
  :ensure nil
  :hook ((dired-mode . dired-omit-mode)
	 (dired-mode . dired-hide-details-mode))
  :bind (:map dired-mode-map
	      ("s-." . dired-omit-mode)
	      ("C-c i" . image-dired)
	      ("s-/ l" . org-store-link))
  :custom
  (dired-omit-verbose nil)
  (dired-omit-files "^\\.[^.].*"))



(with-eval-after-load 'dired
  (defun my/org-attach-visit-headline-from-dired ()
    "Go to the headline corresponding to this org-attach directory."
    (interactive)
    (require 'org-attach)
    (let* ((path (replace-regexp-in-string (regexp-quote org-attach-directory) "" (expand-file-name (dired-filename-at-point))))
           (id-parts (split-string path "/"))
           (id1 (nth 1 id-parts))
           (id2 (nth 2 id-parts))
           (id (concat id1 id2)))
      (let ((m (org-id-find id 'marker)))
	(unless m (user-error "Cannot find entry with ID \"%s\"" id))
	(pop-to-buffer (marker-buffer m))
	(goto-char m)
	(move-marker m nil)
	(org-fold-show-context)))))

(provide 'init-builtin)
;;; init-builtin.el ends here
