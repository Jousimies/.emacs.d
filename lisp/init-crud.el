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
  :hook (meow-normal-mode . ace-pinyin-global-mode))

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

(use-package rime
  :load-path "packages/emacs-rime/"
  :demand t
  :hook (post-command . im-change-cursor-color)
  :bind (:map rime-mode-map
              ("M-j" . rime-force-enable))

  :preface
  (setq rime-title " ")
  :config
  (defvar im-cursor-color "red"
    "The color for input method.")

  (defun im--chinese-p ()
    "Check if the current input state is Chinese."
    (if (featurep 'rime)
        (and (rime--should-enable-p)
             (not (rime--should-inline-ascii-p))
             current-input-method)
      current-input-method))

  (defun im-change-cursor-color ()
    "Set cursor color depending on input method."
    (interactive)
    (set-cursor-color (if (im--chinese-p)
                          im-cursor-color
                        (foreground-color-at-point))))
  (setq default-input-method "rime")
  (setq rime-user-data-dir "~/Library/Rime/")
  (setq rime-emacs-module-header-root "/Applications/Emacs.app/Contents/Resources/include/")
  (setq rime-librime-root (expand-file-name "librime/dist" user-emacs-directory))
  (setq rime-disable-predicates '(rime-predicate-prog-in-code-p
                                  rime-predicate-org-in-src-block-p
                                  rime-predicate-org-latex-mode-p
                                  rime-predicate-tex-math-or-command-p))
  (setq rime-inline-predicates '(rime-predicate-space-after-cc-p
                                 rime-predicate-after-alphabet-char-p)))

(use-package rime-regexp
  :load-path "packages/rime-regexp.el/"
  :hook (minibuffer-mode . rime-regexp-mode))

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
  :commands selected-minor-mode
  :hook (post-select-region . selected-minor-mode)
  :bind (:map selected-keymap
              ("q" . selected-off)
              ("u" . upcase-region)
              ("d" . downcase-region)
              ("c" . kill-ring-save)
              ("x" . kill-region)
              ("w" . count-words-region)
              ("m" . apply-macro-to-region-lines))
  :config
  (with-eval-after-load 'expand-region
    (define-key selected-keymap (kbd "=") #'er/expand-region))
  (with-eval-after-load 'go-translate
    (define-key selected-keymap (kbd "t") #'my/gts-do-translate)))

(provide 'init-crud)
;;; init-crud.el ends here.
