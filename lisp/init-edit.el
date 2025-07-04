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
  :hook (after-init . undo-fu-session-global-mode)
  :custom
  (undo-fu-session-directory (expand-file-name "undo-fu-session/" cache-directory)))

(with-eval-after-load 'undo-fu-session
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
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))

(use-package hungry-delete
  :load-path "packages/hungry-delete/"
  :custom
  (hungry-delete-chars-to-skip " \t\n\r\f\v")
  :hook ((text-mode . hungry-delete-mode)
         (prog-mode . hungry-delete-mode)
         (org-mode . hungry-delete-mode)))

(use-package avy
  :load-path "packages/avy/" "packages/pinyinlib.el/"
  :bind ([remap goto-char] . my/avy-goto-char-timer))
(with-eval-after-load 'avy
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

;; rime 升级后需要同步升级 librime，去下面的链接中下载对应的文件
;; https://github.com/rime/librime/releases
;; 删除 emacs-rime 文件下生成的 .dylib 文件
;; 重新生成 .dylib 文件
(use-package rime
  :load-path "packages/emacs-rime/"
  :config
  ;; (setq default-input-method "rime")
  (setq rime-librime-root (expand-file-name "librime/dist" user-emacs-directory))
  (setq rime-emacs-module-header-root "/Applications/Emacs.app/Contents/Resources/include/")
  (setq rime-user-data-dir "~/Library/Rime/"))

(use-package rime-regexp
  :load-path "packages/rime-regexp.el/" "packages/emacs-rime/"
  :hook (minibuffer-mode . rime-regexp-mode))

(use-package tempel
  :load-path "packages/tempel/"
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert)
	 (:map tempel-map
	       ("<down>" . tempel-next)))
  :custom
  (tempel-path `("~/.emacs.d/template/tempel"
                 ,(expand-file-name "config/tempel" my-galaxy))))
(with-eval-after-load 'tempel
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

(use-package yasnippet
  :load-path "packages/yasnippet/"
  :hook (after-init . yas-global-mode))

(use-package yasnippet-snippets
  :load-path "packages/yasnippet-snippets/"
  :after yasnippet)

(use-package expand-region
  :load-path "packages/expand-region.el/"
  :bind ("C-=" . er/expand-region)
  :config
  (add-to-list 'expand-region-exclude-text-mode-expansions 'org-mode)
  (add-to-list 'expand-region-exclude-text-mode-expansions 'LaTeX-mode))

(use-package surround
  :load-path "packages/surround/"
  :commands surround-delete surround-change surround-insert)

(use-package selected
  :load-path "packages/selected.el/"
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
  :load-path "packages/symbol-overlay/"
  :hook ((prog-mode . symbol-overlay-mode)
         (html-mode . symbol-overlay-mode))
  :bind (:map symbol-overlay-mode-map
              ("M-i" . symbol-overlay-put)
              ("M-I" . symbol-overlay-remove-all)))

(use-package rainbow-mode
  :load-path "packages/rainbow-mode/"
  :hook (prog-mode . rainbow-mode))

;; pulse
(use-package goggles
  :load-path "packages/goggles/"
  :hook ((prog-mode text-mode) . goggles-mode))

;; Use mode-line indicate which buffer cursor located
;; (use-package auto-dim-other-buffers
;;   :load-path "packages/auto-dim-other-buffers.el/"
;;   :hook (after-init . auto-dim-other-buffers-mode)
;;   :config
;;   (setq auto-dim-other-buffers-dim-on-focus-out nil
;;         auto-dim-other-buffers-dim-on-switch-to-minibuffer nil))

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
;; (defun my/move-line (arg)
;;   (interactive)
;;   (let ((col (current-column)))
;;     (unless (eq col 0)
;;       (move-to-column 0))
;;     (save-excursion
;;       (forward-line)
;;       (transpose-lines arg))
;;     (forward-line arg)))

;; (defun my/move-line-down ()
;;   (interactive)
;;   (my/move-line 1))

;; (defun my/move-line-up ()
;;   (interactive)
;;   (my/move-line -1))

;; (global-set-key (kbd "M-P") #'my/move-line-up)
;; (global-set-key (kbd "M-N") #'my/move-line-down)

;; (use-package indent-bars
;;   :load-path "packages/indent-bars/"
;;   :hook (prog-mode . indent-bars-mode)
;;   :config
;;   (require 'indent-bars-ts)
;;   :custom-face
;;   (indent-bars-face ((t (:height 1.08))))
;;   :custom
;;   (indent-bars-treesit-support t)
;;   (indent-bars-no-descend-string t)
;;   (indent-bars-treesit-ignore-blank-lines-types '("module"))
;;   (indent-bars-prefer-character t)
;;   (indent-bars-treesit-wrap
;;    '((python
;; 	  argument_list
;; 	  parameters ; for python, as an example
;; 	  list
;; 	  list_comprehension
;; 	  dictionary
;; 	  dictionary_comprehension
;; 	  parenthesized_expression
;; 	  subscript)))
;;   (indent-bars-no-stipple-char ?\⎸))

(use-package emacs-everywhere
  :load-path "packages/emacs-everywhere/"
  :commands emacs-everywhere)

;; (use-package ultra-scroll
;;   :load-path "packages/ultra-scroll/"
;;   :init
;;   (setq scroll-conservatively 101 ; important!
;;         scroll-margin 0)
;;   :hook (on-first-input . ultra-scroll-mode))

(provide 'init-edit)
;;; init-edit.el ends here
