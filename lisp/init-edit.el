;;; init-edit.el --- Emacs better edit performance   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Duan Ning

;; Author: Duan Ning <jousimies@DNs-Air.local>
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
(use-package autorevert
  :ensure nil
  :hook (on-first-file . global-auto-revert-mode)
  :bind ([remap revert-buffer] . revert-buffer-quick))

(with-eval-after-load 'register
  (setopt register-preview-delay 0)
  (set-register ?a (cons 'file (concat icloud "iCloud~com~appsonthemove~beorg/Documents/org/gtd_archive_" (format-time-string "%Y"))))
  (set-register ?f (cons 'file (concat icloud "iCloud~com~appsonthemove~beorg/Documents/org/flash_thoughts_" (format-time-string "%Y"))))
  (set-register ?t (cons 'file (expand-file-name "iCloud~com~appsonthemove~beorg/Documents/org/org-gtd-tasks.org" icloud)))
  (set-register ?r (cons 'file (expand-file-name (format-time-string "logs/weekly_review_%Y.org") my-galaxy)))
  (set-register ?l (cons 'file (expand-file-name (format-time-string "logs/work_log_%Y.org") my-galaxy))))

(use-package recentf
  :ensure nil
  :hook (on-first-buffer . recentf-mode)
  :custom
  (recentf-save-file (expand-file-name "recentf" cache-directory))
  (recentf-auto-cleanup 300)
  (recentf-max-saved-items 1000)
  (recentf-exclude '("~/.telega")))

(use-package savehist
  :ensure nil
  :hook (on-first-buffer . savehist-mode)
  :custom
  (savehist-file (expand-file-name "history" cache-directory))
  (savehist-additional-variables '(kill-ring
                                   search-ring
                                   regexp-search-ring)))

(use-package saveplace
  :ensure nil
  :hook (on-first-buffer . save-place-mode)
  :custom
  (save-place-file (expand-file-name "places" cache-directory)))

(use-package undo-fu-session
  :hook (on-first-buffer . undo-fu-session-global-mode)
  :custom
  (undo-fu-session-directory (expand-file-name "undo-fu-session/" cache-directory)))

(use-package vundo
  :bind ("s-z" . vundo)
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))

(use-package hungry-delete
  :custom
  (hungry-delete-chars-to-skip " \t\n\r\f\v")
  :hook ((text-mode . hungry-delete-mode)
         (prog-mode . hungry-delete-mode)
         (org-mode . hungry-delete-mode)))

(use-package ace-pinyin
  :bind (([remap goto-char] . ace-pinyin-jump-char)
         ("M-g C" . ace-pinyin-jump-char-2)
         ("M-g ." . ace-pinyin-jump-char-in-line)
         ("M-g w" . ace-pinyin-jump-word)))

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

(use-package electric
  :ensure nil
  :hook ((prog-mode . (lambda ()
                        (progn
                          (electric-indent-local-mode 1)
                          (electric-pair-local-mode 1))))
		 (org-mode . (lambda ()
					   (progn
						 (electric-pair-mode -1)
						 (electric-quote-mode -1)
						 (electric-indent-mode -1))))))

(use-package delsel
  :hook (on-first-buffer . delete-selection-mode))

(use-package rime-regexp
  :load-path "packages/rime-regexp.el/" "packages/emacs-rime/"
  :hook (minibuffer-mode . rime-regexp-mode)
  :config
  (setq rime-librime-root (expand-file-name "librime/dist" user-emacs-directory))
  (setq rime-emacs-module-header-root "/Applications/Emacs.app/Contents/Resources/include/")
  (setq rime-user-data-dir "~/Library/Rime/"))

(use-package tempel
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert)
		 (:map tempel-map
			   ("<down>" . tempel-next)))
  :custom
  (tempel-path `("~/.emacs.d/template/tempel"
                 ,(expand-file-name "config/tempel" my-galaxy))))

(use-package yasnippet
  :hook ((prog-mode . yas-minor-mode)
		 (org-mode . yas-minor-mode)))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package expand-region
  :bind ("C-=" . er/expand-region)
  :config
  (add-to-list 'expand-region-exclude-text-mode-expansions 'org-mode)
  (add-to-list 'expand-region-exclude-text-mode-expansions 'LaTeX-mode))

(use-package surround)

(use-package selected
  :hook (post-select-region . selected-minor-mode)
  :bind (:map selected-keymap
              ("q" . selected-off)
              ("x" . kill-region)
              ("w" . count-words-region)
              ("i" . surround-insert)
			  ("d" . surround-delete)
              ("s" . my/search-menu)
              ("m" . apply-macro-to-region-lines)
              ("\\" . indent-region)
              (";" . comment-dwim)))

(defun my/copy-region ()
  (interactive)
  (if (eq major-mode 'xwidget-webkit-mode)
      (xwidget-webkit-copy-selection-as-kill)
    (ns-copy-including-secondary)))
(global-set-key (kbd "s-c") #'my/copy-region)

(use-package symbol-overlay
  :hook ((prog-mode . symbol-overlay-mode)
         (html-mode . symbol-overlay-mode))
  :bind (:map symbol-overlay-mode-map
              ("M-i" . symbol-overlay-put)
              ("M-I" . symbol-overlay-remove-all)))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

;; pulse
;; Temporarily highlights the line, which is sometimes useful for orientation after a navigation command.
;; Similar packages `nav-flash' `beacon' `Pulsar'.
(defun my-pulse-momentary (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point) 'next-error))

(defun my-recenter (&rest _)
  "Recenter and pulse the current line."
  (recenter)
  (my-pulse-momentary))

(dolist (cmd '(recenter-top-bottom
               other-window windmove-do-window-select
               pop-to-mark-command pop-global-mark
               pager-page-down pager-page-up
               ace-window
               my/yank))
  (advice-add cmd :after #'my-pulse-momentary))
(add-hook 'bookmark-after-jump-hook #'my-recenter)
(add-hook 'next-error-hook #'my-recenter)
(add-hook 'other-window-hook #'my-recenter)
(add-hook 'imenu-after-jump-hook #'my-recenter)

;; Make region read-only or writable
(defun make-region-read-only (beg end)
  (interactive "r")
  (let ((inhibit-read-only t))
	(with-silent-modifications
	  (add-text-properties beg end '(read-only t)))))

(defun make-region-writable (beg end)
  (interactive "r")
  (let ((inhibit-read-only t))
	(with-silent-modifications
	  (remove-text-properties beg end '(read-only t)))))

;; https://github.com/takeokunn/.emacs.d/blob/main/index.org
(defun my/move-line (arg)
  (interactive)
  (let ((col (current-column)))
    (unless (eq col 0)
      (move-to-column 0))
    (save-excursion
      (forward-line)
      (transpose-lines arg))
    (forward-line arg)))

(defun my/move-line-down ()
  (interactive)
  (my/move-line 1))

(defun my/move-line-up ()
  (interactive)
  (my/move-line -1))

(global-set-key (kbd "M-P") #'my/move-line-up)
(global-set-key (kbd "M-N") #'my/move-line-down)


(provide 'init-edit)
;;; init-edit.el ends here
