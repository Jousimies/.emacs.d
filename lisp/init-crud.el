;; init-crud.el --- Add, delte, modify and check. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package undo-fu-session
  :load-path "packages/undo-fu-session/"
  :hook (after-init . undo-fu-session-global-mode)
  :config
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
  :hook (after-init . savehist-mode)
  :config
  (setq history-length 1000
        savehist-additional-variables '(kill-ring
                                        search-ring
                                        regexp-search-ring)
        history-delete-duplicates t))

(use-package saveplace
  :hook (after-init . save-place-mode))

(use-package recentf
  :hook (after-init . recentf-mode)
  :bind ("C-c f r" . recentf-open)
  :config
  (setq recentf-auto-cleanup 300)
  (setq recentf-max-saved-items 1000)
  (setq recentf-exclude '(".pdf$")))

(use-package register
  :bind ("C-x r j" . jump-to-register)
  :custom
  (register-preview-delay 0)
  :config
  (set-register ?g (cons 'file (expand-file-name "iCloud~com~appsonthemove~beorg/Documents/org/org-gtd-tasks.org" mobile-document)))
  (set-register ?b (cons 'file (expand-file-name "denote/books/20230301T211439--Book-lists-and-reading-record__reading.org" my-galaxy)))
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

(defvar cur-sys-input-method nil)

;;;###autoload
(defun switch-to-abc-input-method ()
  "Switch to the ABC input method."
  (interactive)
  (setq cur-sys-input-method nil)
  (call-process "im-select" nil nil nil "com.apple.keylayout.ABC")
  (setq cursor-type 'box)
  (set-cursor-color (foreground-color-at-point))
  (force-mode-line-update))

;;;###autoload
(defun switch-to-squirrel-input-method ()
  "Switch to the Squirrel input method (Hans)."
  (interactive)
  (setq cur-sys-input-method t)
  (call-process "im-select" nil nil nil "im.rime.inputmethod.Squirrel.Hans")
  (setq cursor-type 'bar)
  (set-cursor-color 'red)
  (force-mode-line-update))

;;;###autoload
(defun toggle-sys-input-method ()
  (interactive)
  (if cur-sys-input-method
      (switch-to-abc-input-method)
    (switch-to-squirrel-input-method)))

(add-hook 'minibuffer-mode-hook 'switch-to-abc-input-method)
(add-hook 'find-file-hook 'switch-to-abc-input-method)
(add-hook 'isearch-mode-end-hook 'switch-to-abc-input-method)
(add-hook 'quit-window-hook (lambda ()
                              (when cur-sys-input-method
                                (switch-to-abc-input-method))))

(global-set-key (kbd "C-\\") 'toggle-sys-input-method)

(use-package yasnippet
  :load-path "packages/yasnippet/"
  :hook (minibuffer-mode . yas-global-mode))

(use-package yasnippet-snippets
  :load-path "packages/yasnippet-snippets/"
  :after yasnippet)

(use-package expand-region
  :load-path "packages/expand-region.el/"
  :bind ("C-=" . er/expand-region))

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

(provide 'init-crud)
;;; init-crud.el ends here.

;; (dolist (hook '(prog-mode-hook html-mode-hook yaml-mode-hook conf-mode-hook))
;;   (add-hook hook 'symbol-overlay-mode))

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

;; (with-eval-after-load 'symbol-overlay
;;   (define-key symbol-overlay-mode-map (kbd "M-i") 'symbol-overlay-put)
;;   (define-key symbol-overlay-mode-map (kbd "M-I") 'symbol-overlay-remove-all)
;;   (define-key symbol-overlay-mode-map (kbd "M-n") 'symbol-overlay-jump-next)
;;   (define-key symbol-overlay-mode-map (kbd "M-p") 'symbol-overlay-jump-prev)
;;   (define-key symbol-overlay-mode-map (kbd "s-r") 'symbol-overlay-rename))
