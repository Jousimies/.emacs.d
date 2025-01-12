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

;; auto-revert
(add-hook 'on-first-file-hook #'global-auto-revert-mode)

;; register
(with-eval-after-load 'register
  (setopt register-preview-delay 0)
  (set-register ?a (cons 'file (concat icloud "iCloud~com~appsonthemove~beorg/Documents/org/gtd_archive_" (format-time-string "%Y"))))
  (set-register ?f (cons 'file (concat icloud "iCloud~com~appsonthemove~beorg/Documents/org/flash_thoughts_" (format-time-string "%Y"))))
  (set-register ?t (cons 'file (expand-file-name "iCloud~com~appsonthemove~beorg/Documents/org/org-gtd-tasks.org" icloud)))
  (set-register ?r (cons 'file (expand-file-name (format-time-string "logs/weekly_review_%Y.org") my-galaxy)))
  (set-register ?l (cons 'file (expand-file-name (format-time-string "logs/work_log_%Y.org") my-galaxy))))

;; recentf
(add-hook 'after-init-hook #'recentf-mode)
(with-eval-after-load 'recentf
  (setopt recentf-save-file (expand-file-name "recentf" cache-directory)
	  recentf-auto-cleanup 300
	  recentf-max-saved-items 1000
	  recentf-exclude '("~/.telega")))

;; savehist
(add-hook 'on-first-file-hook #'savehist-mode)
(with-eval-after-load 'savehist
  (setopt savehist-file (expand-file-name "history" cache-directory)
	  history-length 1000
          savehist-additional-variables '(kill-ring
                                          search-ring
                                          regexp-search-ring)
          history-delete-duplicates t))

;; save-place-mode
(add-hook 'on-first-file-hook #'save-place-mode)
(setopt save-place-file (expand-file-name "places" cache-directory))

;; undo-fu-session
(use-package undo-fu-session
  :straight t
  :hook (on-first-file . undo-fu-session-global-mode)
  :config
  (setq undo-fu-session-directory (expand-file-name "undo-fu-session/" cache-directory))
  (defun my/undo-fu-session--make-file-name (filename)
    "Take the path FILENAME and return a name base on this."
    (concat
     (file-name-concat undo-fu-session-directory
                       (md5 (convert-standard-filename (expand-file-name filename))))
     (undo-fu-session--file-name-ext)))
  (advice-add 'undo-fu-session--make-file-name :override #'my/undo-fu-session--make-file-name))

(use-package vundo
  :straight t
  :bind ("s-z" . vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package hungry-delete
  :straight t
  :custom
  (hungry-delete-chars-to-skip " \t\n\r\f\v")
  :hook ((text-mode . hungry-delete-mode)
         (prog-mode . hungry-delete-mode)
         (org-mode . hungry-delete-mode)))

(use-package avy
  :straight t
  :bind ([remap goto-char] . my/avy-goto-char-timer)
  :config
  (defun my/avy-goto-char-timer (&optional arg)
    (interactive "P")
    (unless (featurep 'pinyinlib)
      (require 'pinyinlib))
    (let ((avy-all-windows (if arg
                               (not avy-all-windows)
                             avy-all-windows)))
      (avy-with avy-goto-char-timer
		(setq avy--old-cands (avy--read-candidates
				      'pinyinlib-build-regexp-string))
		(avy-process avy--old-cands))))

  (advice-add 'avy-goto-char-timer :override #'my/avy-goto-char-timer))

(use-package hippie-exp
  :bind ([remap dabbrev-expand] . hippie-expand)
  :config
  (setq hippie-expand-try-functions-list '(try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-all-abbrevs
                                           try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill
                                           try-complete-lisp-symbol-partially
                                           try-complete-lisp-symbol)))

(add-hook 'after-init-hook #'electric-pair-mode)
(add-hook 'after-init-hook #'electric-quote-mode)
(add-hook 'after-init-hook #'electric-indent-mode)
(add-hook 'after-init-hook #'delete-selection-mode)

(use-package rime-regexp
  :straight (rime-regexp :host github :repo "colawithsauce/rime-regexp.el")
  :hook (minibuffer-mode . rime-regexp-mode)
  :config
  (setq rime-librime-root (expand-file-name "librime/dist" user-emacs-directory))
  (setq rime-emacs-module-header-root "/Applications/Emacs.app/Contents/Resources/include/")
  (setq rime-user-data-dir "~/Library/Rime/"))

(use-package tempel
  :straight t
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert)
	 (:map tempel-map
	       ("<down>" . tempel-next)))
  :config
  (setq tempel-path `("~/.emacs.d/template/tempel"
                      ,(expand-file-name "config/tempel" my-galaxy))))

(use-package yasnippet
  :straight t
  :hook (minibuffer-mode . yas-global-mode)
  :config
  (use-package yasnippet-snippets
    :straight t))

(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region)
  :config
  (add-to-list 'expand-region-exclude-text-mode-expansions 'org-mode)
  (add-to-list 'expand-region-exclude-text-mode-expansions 'LaTeX-mode))

(use-package surround
  :straight t
  :defer t)

(use-package selected
  :straight t
  :hook (post-select-region . selected-minor-mode)
  :bind (:map selected-keymap
              ("q" . selected-off)
              ("x" . kill-region)
              ("w" . count-words-region)
              ("i" . surround-insert)
              ("c" . surrond-change)
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
  :straight t
  :hook ((prog-mode . symbol-overlay-mode)
         (html-mode . symbol-overlay-mode))
  :bind (:map symbol-overlay-mode-map
              ("M-i" . symbol-overlay-put)
              ("M-I" . symbol-overlay-remove-all)))

(use-package rainbow-mode
  :straight t
  :hook (prog-mode . rainbow-mode))

;; pulse
(use-package pulse
  :custom-face
  (pulse-highlight-start-face ((t (:inherit region :background unspecified))))
  (pulse-highlight-face ((t (:inherit region :background unspecified :extend t))))
  :hook (((dumb-jump-after-jump imenu-after-jump) . +recenter-and-pulse)
         ((bookmark-after-jump magit-diff-visit-file next-error) . +recenter-and-pulse-line))
  :init
  (setq pulse-delay 0.1
        pulse-iterations 2)

  (defun +pulse-momentary-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))

  (defun +pulse-momentary (&rest _)
    "Pulse the region or the current line."
    (if (fboundp 'xref-pulse-momentarily)
        (xref-pulse-momentarily)
      (+pulse-momentary-line)))

  (defun +recenter-and-pulse(&rest _)
    "Recenter and pulse the region or the current line."
    (recenter)
    (+pulse-momentary))

  (defun +recenter-and-pulse-line (&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (+pulse-momentary-line))

  (dolist (cmd '(recenter-top-bottom
                 other-window switch-to-buffer
                 aw-select toggle-window-split
                 windmove-do-window-select
                 pager-page-down pager-page-up
                 treemacs-select-window
                 tab-bar-select-tab))
    (advice-add cmd :after #'+pulse-momentary-line))

  (dolist (cmd '(pop-to-mark-command
                 pop-global-mark
                 goto-last-change))
    (advice-add cmd :after #'+recenter-and-pulse))

  (dolist (cmd '(symbol-overlay-basic-jump
                 compile-goto-error))
    (advice-add cmd :after #'+recenter-and-pulse-line)))

(use-package goggles
  :straight t
  :hook ((prog-mode text-mode) . goggles-mode))

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

(use-package emacs-everywhere
  :straight t
  :commands emacs-everywhere-mode)

(provide 'init-edit)
;;; init-edit.el ends here
