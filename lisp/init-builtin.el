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

(add-hook 'on-first-buffer-hook #'column-number-mode)
(add-hook 'on-first-file-hook #'size-indication-mode)
(add-hook 'org-mode-hook #'visual-line-mode)
(add-hook 'eww-mode #'visual-line-mode)

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
		multisession-directory (expand-file-name "multisession" cache-directory)
		auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" cache-directory))

;; (setq backup-directory-alist '(("." . "~/.emacs.d/cache/backups")))
(add-hook 'on-first-file-hook #'auto-save-visited-mode)

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

(add-hook 'on-first-file-hook #'global-so-long-mode)
(add-hook 'on-first-file-hook #'global-prettify-symbols-mode)
(add-hook 'on-first-file-hook #'global-word-wrap-whitespace-mode)
(add-hook 'after-init-hook 'display-battery-mode)
(add-hook 'after-init-hook 'display-time-mode)

(with-eval-after-load 'time
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

(add-hook 'on-first-buffer-hook #'midnight-mode)

(setopt prettify-symbols-alist '(("lambda" . ?λ)
				 ("function" . ?𝑓)))

(add-hook 'minibuffer-mode-hook #'minibuffer-electric-default-mode)

;;Bookmark
(setq bookmark-default-file (expand-file-name "bookmarks" cache-directory))

;; auto-insert-mode
(add-hook 'on-first-file-hook #'auto-insert-mode)
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

(setopt display-line-numbers-widen t
		display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'org-mode-hook #'display-line-numbers-mode)

(face-spec-set 'fill-column-indicator
                 '((default :height 0.1))
                 'face-override-spec)
(setq-default fill-column 90)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; (define-fringe-bitmap 'right-curly-arrow  [])
;; (define-fringe-bitmap 'left-curly-arrow  [])
;; https://xenodium.com/toggling-emacs-continuation-fringe-indicator/
(setq-default fringe-indicator-alist
              (delq (assq 'continuation fringe-indicator-alist) fringe-indicator-alist))
(defun toggle-continuation-fringe-indicator ()
  (interactive)
  (setq-default
   fringe-indicator-alist
   (if (assq 'continuation fringe-indicator-alist)
       (delq (assq 'continuation fringe-indicator-alist) fringe-indicator-alist)
     (cons '(continuation right-curly-arrow left-curly-arrow) fringe-indicator-alist))))

(add-hook 'find-file-hook #'show-paren-mode)
(with-eval-after-load 'paren
  (setopt show-paren-style 'parenthesis
	  show-paren-context-when-offscreen 'overlay
	  show-paren-highlight-openparen t
	  show-paren-when-point-inside-paren t
	  show-paren-when-point-in-periphery t))

(when window-divider-mode
  (setopt window-divider-default-bottom-width 1
	  window-divider-default-places 'bottom-only))

(add-hook 'find-file-hook #'winner-mode)
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
(global-set-key (kbd "M-g u") #'winner-undo)
(global-set-key (kbd "M-g r") #'winner-redo)

(add-to-list 'display-buffer-alist '("\\*Outline"
                                       (display-buffer-in-side-window)
                                       (side . right)
                                       (window-width . 0.5)))

(add-hook 'window-setup-hook #'windmove-mode)
(global-set-key (kbd "M-g h") #'windmove-left)
(global-set-key (kbd "M-g l") #'windmove-right)
(global-set-key (kbd "M-g k") #'windmove-up)
(global-set-key (kbd "M-g j") #'windmove-down)

(setopt switch-to-buffer-in-dedicated-window 'pop
	switch-to-buffer-obey-display-actions t)

(add-hook 'after-init-hook 'pixel-scroll-mode)

;; desktop
;; (add-hook 'on-first-buffer-hook #'desktop-save-mode)
;; (setq desktop-path `(,cache-directory))
;; (add-hook 'after-init-hook #'desktop-read)

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

;; Repeatable key chords (repeat-mode)
(add-hook 'emacs-startup-hook #'repeat-mode)
(with-eval-after-load 'repeat
  (setopt repeat-on-final-keystroke t
	  repeat-exit-timeout 5
	  repeat-exit-key "<escape>"
	  repeat-keep-prefix nil
	  set-mark-command-repeat-pop t))

;; hl-line-mode
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'package-menu-mode #'hl-line-mode)
(with-eval-after-load 'hl-line
  (setopt hl-line-sticky-flag nil))

;; calendar
(add-hook 'calendar-today-visible-hook #'calendar-mark-today)
(with-eval-after-load 'calendar
  (setopt calendar-view-diary-initially-flag t
	  calendar-mark-diary-entries-flag t
	  calendar-date-style 'iso
	  calendar-date-display-form calendar-iso-date-display-form
	  diary-date-forms diary-iso-date-forms
	  calendar-time-display-form '(24-hours ":" minutes
						(when time-zone
						  (format "(%s)" time-zone)))))
(global-set-key (kbd "C-x c") #'calendar)


(use-package appt
  :hook (diary-mode . appt-activate)
  :config
  (setq appt-display-diary nil)
  (setq appt-disp-window-function #'appt-disp-window)
  (setq appt-display-mode-line t)
  (setq appt-display-interval 3)
  (setq appt-audible nil)
  (setq appt-warning-time-regexp "appt \\([0-9]+\\)")
  (setq appt-message-warning-time 6))

(use-package diary-lib
  :defer t
  :config
  (add-hook 'diary-list-entries-hook #'diary-sort-entries)
  (add-hook 'diary-mode-hook #'goto-address-mode)
  (setq diary-display-function #'diary-fancy-display)
  (setq diary-header-line-format nil)
  (setq diary-list-include-blanks nil)
  (setq diary-abbreviated-year-flag nil)
  (setq diary-number-of-entries 7)
  (setq diary-comment-start ");;")
  (setq diary-comment-end "")
  (setq diary-nonmarking-symbol "!")

  (setq diary-file (expand-file-name "logs/diary.org" my-galaxy)))


;; Tramp
(with-eval-after-load 'tramp
 (setq tramp-persistency-file-name (expand-file-name "tramp" cache-directory)))


(provide 'init-builtin)
;;; init-builtin.el ends here
