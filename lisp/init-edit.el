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

(use-package undo-fu-session
  :load-path "packages/undo-fu-session/"
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
  :load-path "packages/vundo/"
  :bind ("s-z" . vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package savehist
  :hook (on-first-file . savehist-mode)
  :config
  (setq savehist-file (expand-file-name "history" cache-directory)
		history-length 1000
        savehist-additional-variables '(kill-ring
                                        search-ring
                                        regexp-search-ring)
        history-delete-duplicates t))

(use-package saveplace
  :hook (on-first-file . save-place-mode)
  :config
  (setq save-place-file (expand-file-name "places" cache-directory)))

(use-package recentf
  :hook (on-first-input . recentf-mode)
  :bind ("C-c f r" . recentf-open)
  :config
  (setq recentf-save-file (expand-file-name "recentf" cache-directory))
  (setq recentf-auto-cleanup 300)
  (setq recentf-max-saved-items 1000)
  (setq recentf-exclude '(".pdf$")))

(use-package register
  :bind ("C-x r j" . jump-to-register)
  :config
  (setq register-preview-delay 0)
  (set-register ?g (cons 'file (expand-file-name "iCloud~com~appsonthemove~beorg/Documents/org/org-gtd-tasks.org" mobile-document)))
  (set-register ?b (cons 'file (expand-file-name "denote/books/20230301T211439--Book-lists-and-reading-record__reading.org" my-galaxy)))
  (set-register ?l (cons 'file (concat my-galaxy "/logs/log_" (format-time-string "%Y") ".org")))
  (set-register ?f (cons 'file (expand-file-name "finance/beans/finance.bean" my-galaxy))))

(use-package autorevert
  :hook (text-mode . global-auto-revert-mode))

(use-package hungry-delete
  :load-path "packages/hungry-delete/"
  :custom
  (hungry-delete-chars-to-skip " \t\n\r\f\v")
  :hook ((text-mode . hungry-delete-mode)
         (prog-mode . hungry-delete-mode)
         (org-mode . hungry-delete-mode)))

(use-package ace-pinyin
  :load-path ("packages/ace-pinyin/" "packages/avy/" "packages/pinyinlib.el")
  :bind ([remap goto-char] . ace-pinyin-jump-char-2))

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

(add-hook 'prog-mode-hook 'electric-pair-local-mode)
(add-hook 'org-mode-hook 'electric-pair-local-mode)
(add-hook 'org-mode-hook
          (lambda ()
            (require 'elec-pair)
            (setq-local electric-pair-inhibit-predicate
                        `(lambda (c)
                           (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

(use-package delsel
  :hook (text-mode . delete-selection-mode))

(use-package tempel
  :load-path "packages/tempel/"
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert))
  :config
  (setq tempel-path `("~/.emacs.d/template/tempel"
                      ,(expand-file-name "config/tempel" my-galaxy))))

(use-package macim
  :load-path "~/.emacs.d/packages/macim.el/"
  :bind (("C-\\" . macim-switch)
         :map isearch-mode-map
         ("C-\\" . macim-switch))
  :hook ((emacs-startup . macim-select-ascii)
         (emacs-startup . macim-mode)
         (isearch-mode . macim-select-ascii)
         (minibuffer-mode . macim-select-ascii))
  :config
  (setq macim-other "im.rime.inputmethod.Squirrel.Hans")
  (defun macim-switch ()
    (interactive)
    (if current-system-input-method
        (progn
          (macim-select-ascii)
          (force-mode-line-update))
      (progn
        (macim-select-other)
        (force-mode-line-update)))))

(use-package emt
  :load-path "packages/emt"
  :bind (("M-f" . emt-forward-word)
         ("M-b" . emt-backward-word)
         ("M-d" . emt-kill-word)
         ("M-h" . emt-backward-kill-word))
  :hook (on-first-input . emt-ensure))

(use-package yasnippet
  :load-path "packages/yasnippet/"
  :hook (minibuffer-mode . yas-global-mode)
  :config
  (use-package yasnippet-snippets
    :load-path "packages/yasnippet-snippets/"))

(use-package expand-region
  :load-path "packages/expand-region.el/"
  :bind ("C-=" . er/expand-region)
  :config
  (add-to-list 'expand-region-exclude-text-mode-expansions 'org-mode)
  (add-to-list 'expand-region-exclude-text-mode-expansions 'LaTeX-mode))

(use-package selected
  :load-path "packages/selected.el/"
  :hook (post-select-region . selected-minor-mode)
  :bind (:map selected-keymap
              ("q" . selected-off)
              ("u" . upcase-region)
              ("d" . downcase-region)
              ("c" . my/copy-region)
              ("x" . kill-region)
              ("w" . count-words-region)
              ("i" . surround-insert)
              ("s" . my/search)
              ("t" . my/gts-do-translate)
              ("m" . apply-macro-to-region-lines)
              ("\\" . indent-region)
              (";" . comment-dwim)
              ("+" . my/add-symbol-to-region))
  :config
  (defun my/copy-region ()
    (interactive)
    (if (eq major-mode 'xwidget-webkit-mode)
        (xwidget-webkit-copy-selection-as-kill)
      (copy-region-as-kill (region-beginning) (region-end)))))

(use-package symbol-overlay
  :load-path "packages/symbol-overlay/"
  :hook ((prog-mode . symbol-overlay-mode)
         (html-mode . symbol-overlay-mode))
  :bind (:map symbol-overlay-mode-map
              ("M-i" . symbol-overlay-put)
              ("M-I" . symbol-overlay-remove-all)
              ("M-n" . symbol-overlay-jump-next)
              ("M-p" . symbol-overlay-jump-prev)
              ("M-%" . symbol-overlay-query-replace)))

(use-package rainbow-mode
  :load-path "packages/rainbow-mode/"
  :hook (prog-mode . rainbow-mode))

;; pulse
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

(use-package surround
  :load-path "packages/surround/"
  :bind-keymap ("M-'" . surround-keymap))

(provide 'init-edit)
;;; init-edit.el ends here