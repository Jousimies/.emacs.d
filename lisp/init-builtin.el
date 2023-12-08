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

;; files
(global-set-key (kbd "C-c b s") #'scratch-buffer)
(global-set-key (kbd "C-h") #'delete-backward-char)
(global-set-key (kbd "M-h") #'backward-kill-word)
(global-set-key (kbd "<f1>") #'help-command)

(add-hook 'on-first-buffer-hook #'column-number-mode)
(add-hook 'on-first-file-hook #'size-indication-mode)
(add-hook 'org-mode-hook #'visual-line-mode)

(setopt mark-ring-max 128
        kill-do-not-save-duplicates t
        kill-ring-max (* kill-ring-max 2)
        async-shell-command-display-buffer nil)

;; auto-save
(setopt trash-directory "~/.Trash"
        auto-save-default nil
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

(global-set-key (kbd "C-c f f") #'find-file-at-point)

(defun switch-to-message ()
    "Quick switch to `*Message*' buffer."
    (interactive)
    (switch-to-buffer "*Messages*"))
(global-set-key (kbd "C-c b m") #'switch-to-message)

(setopt message-kill-buffer-on-exit t
        message-kill-buffer-query nil
        message-sendmail-envelope-from 'header
        message-sendmail-extra-arguments '("-a" "outlook"))

(add-hook 'on-first-file-hook #'global-so-long-mode)
(add-hook 'on-first-file-hook #'global-prettify-symbols-mode)
(add-hook 'on-first-file-hook #'global-word-wrap-whitespace-mode)
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
;; (add-hook 'org-mode-hook #'display-line-numbers-mode)

(face-spec-set 'fill-column-indicator
                 '((default :height 0.1))
                 'face-override-spec)
(setq-default fill-column 90)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(setopt show-paren-style 'parenthesis
		show-paren-context-when-offscreen 'overlay)
(add-hook 'on-first-buffer-hook #'show-paren-mode)

(add-hook 'on-first-buffer-hook (lambda ()
								  (blink-cursor-mode -1)))
(setopt window-divider-default-bottom-width 1
		window-divider-default-places 'bottom-only)

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
                                "*esh command on file*"))
(add-hook 'on-first-buffer-hook #'winner-mode)

(add-to-list 'display-buffer-alist '("\\*Outline"
                                       (display-buffer-in-side-window)
                                       (side . right)
                                       (window-width . 0.5)))
(add-hook 'on-first-buffer-hook #'windmove-mode)
(global-set-key (kbd "C-c <up>") #'windmove-up)
(global-set-key (kbd "C-c <down>") #'windmove-down)

(setopt switch-to-buffer-in-dedicated-window 'pop
		switch-to-buffer-obey-display-actions t)

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


(provide 'init-builtin)
;;; init-builtin.el ends here
