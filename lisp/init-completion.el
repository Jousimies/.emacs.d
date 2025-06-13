;;; init-completion.el --- Completion UI             -*- lexical-binding: t; -*-

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

;; use `M-j' call `icomplete-fido-exit' to exit minibuffer completion.
;; re-use vertico-mode instead of `icomplete-fido-mode'.
;; Due to icomplete has compatible problem with citar, a references manager.
;; use `M-RET' to exit minibuffer input.

;; (use-package icomplete
;;   :hook (on-first-input . icomplete-mode)
;;   :custom
;;   (icomplete-in-buffer t))

(use-package vertico
  :load-path "packages/vertico/"
  :hook (after-init . vertico-mode))

(use-package vertico-directory
  :load-path "packages/vertico/extensions/"
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :bind (:map vertico-map
	      ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
	      ("C-h" . vertico-directory-up)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package orderless
  :load-path "packages/orderless/"
  :custom
  (orderless-matching-styles '(orderless-prefixes orderless-regexp))
  (completion-styles '(basic substring initials orderless))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles . (basic partial-completion orderless)))
     (bookmark (styles . (basic substring)))
     (library (styles . (basic substring)))
     (embark-keybinding (styles . (basic substring)))
     (imenu (styles . (basic substring orderless)))
     (consult-location (styles . (basic substring orderless)))
     (kill-ring (styles . (emacs22 orderless)))
     (eglot (styles . (emacs22 substring orderless))))))

;; https://emacs-china.org/t/macos-save-silently-t/24086
(setq inhibit-message-regexps '("^Saving" "^Wrote"))
(setq set-message-functions '(inhibit-message))

(defun crm-indicator (args)
  (cons (format "[`completing-read-multiple': %s]  %s"
                (propertize
                 (replace-regexp-in-string
                  "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                  crm-separator)
                 'face 'error)
                (car args))
        (cdr args)))

(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

(autoload 'list-colors-duplicates "facemenu")
(use-package consult
  :load-path "packages/consult/"
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (([remap apropos] . consult-apropos)
         ([remap bookmark-jump] . consult-bookmark)
         ([remap goto-line] . consult-line)
         ([remap locate] . consult-locate)
         ([remap load-theme] . consult-theme)
         ([remap man] . consult-man)
         ([remap recentf-open-files] . consult-recent-file)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap yank-pop] . consult-yank-pop)
	 ("M-g l" . consult-goto-line)
         :map minibuffer-mode-map
         ("C-r" . consult-history))
  :custom
  (consult-narrow-key "<")
  :config
  (defvar consult-colors-history nil
    "History for `consult-colors-emacs' and `consult-colors-web'.")

  (defun consult-colors-emacs (color)
    "Show a list of all supported colors for a particular frame.

You can insert the name (default), or insert or kill the hexadecimal or RGB
value of the selected COLOR."
    (interactive
     (list (consult--read (list-colors-duplicates (defined-colors))
                          :prompt "Emacs color: "
                          :require-match t
                          :category 'color
                          :history '(:input consult-colors-history))))
    (insert color))

  (defun consult-colors--web-list nil
    "Return list of CSS colors for `counsult-colors-web'."
    (require 'shr-color)
    (sort (mapcar #'downcase (mapcar #'car shr-color-html-colors-alist)) #'string-lessp))

  (defun consult-colors-web (color)
    "Show a list of all CSS colors.\

You can insert the name (default), or insert or kill the hexadecimal or RGB
value of the selected COLOR."
    (interactive
     (list (consult--read (consult-colors--web-list)
                          :prompt "Color: "
                          :require-match t
                          :category 'color
                          :history '(:input consult-colors-history))))
    (insert color)))
;; https://takeonrules.com/2024/06/08/adding-a-consult-function-for-visualizing-xref/
;; Adding a Consult Function for Visualizing Xref
;; (defvar consult--xref-history nil
;;   "History for the `consult-recent-xref' results.")

;; (defun consult-recent-xref (&optional markers)
;;   "Jump to a marker in MARKERS list (defaults to `xref--history'.

;; The command supports preview of the currently selected marker position.
;; The symbol at point is added to the future history."
;;   (interactive)
;;   (consult--read
;;     (consult--global-mark-candidates
;;       (or markers (flatten-list xref--history)))
;;     :prompt "Go to Xref: "
;;     :annotate (consult--line-prefix)
;;     :category 'consult-location
;;     :sort nil
;;     :require-match t
;;     :lookup #'consult--lookup-location
;;     :history '(:input consult--xref-history)
;;     :add-history (thing-at-point 'symbol)
;;     :state (consult--jump-state)))


(with-eval-after-load 'consult
  (defun my/consult-find-attach ()
    (interactive)
    (let* ((dir (expand-file-name "attach" my-galaxy)))
      (consult-find dir))))

(use-package consult-imenu
  :bind ([remap imenu] . consult-imenu))

(use-package consult-org
  :after org
  :bind (:map org-mode-map
	      ("M-g h" . consult-org-heading)))
;; (use-package consult-omni
;;   :load-path "packages/consult-omni/" "packages/consult-omni/sources"
;;   :after consult
;;   :custom
;;   ;; General settings that apply to all sources
;;   (consult-omni-show-preview t) ;;; show previews
;;   (consult-omni-preview-key "C-o") ;;; set the preview key to C-o
;;   :config
;;   ;; Load Sources Core code
;;   (require 'consult-omni-sources)
;;   ;; Load Embark Actions
;;   (require 'consult-omni-embark)

;;   (consult-omni-sources-load-modules)
;;   (setq consult-omni-multi-sources '("calc"
;;                                      "File"
;;                                      "Buffer"
;;                                      "Bookmark"
;;                                      "Apps"
;;                                      ;; "gptel"
;;                                      ;; "Brave"
;;                                      "Dictionary"
;;                                      ;; "Google"
;;                                      "Wikipedia"
;;                                      "elfeed"
;;                                      "mu4e"
;;                                      ;; "buffers text search"
;;                                      "Notes Search"
;;                                      "Org Agenda"
;;                                      "GitHub"
;;                                      ;; "YouTube"
;;                                      ;; "Invidious"))
;; 									 )))

(use-package marginalia
  :load-path "packages/marginalia/"
  :hook (minibuffer-setup . marginalia-mode))

(use-package embark
  :load-path "packages/embark/"
  :bind (([remap describe-bindings] . embark-bindings)
         ("C-;" . embark-act)
         ("M-." . embark-dwim)
         (:map minibuffer-local-map
               ("C-;" . embark-act)
               ("C-c C-e" . embark-export)
               ("C-c C-l" . embark-collect))))

;; (use-package completion-preview
;;   :hook (on-first-input . global-completion-preview-mode)
;;   :custom
;;   (completion-preview-idle-delay 1)
;;   (completion-preview-ignore-case t)
;;   (completion-preview-minimum-symbol-length 3))

(use-package corfu
  :load-path "packages/corfu/"
  :hook (after-init . global-corfu-mode)
  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  :bind ("M-/" . completion-at-point)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.2)
  (corfu-preselect 'valid)
  (corfu-max-width 120)
  (corfu-on-exact-match nil)
  (corfu-quit-no-match t)
  (global-corfu-modes '((not erc-mode
			     circe-mode
			     help-mode
			     gud-mode
			     vterm-mode)
			t))
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p)
  :config
  (keymap-unset corfu-map "RET"))

(use-package corfu-echo
  :load-path "packages/corfu/extensions/"
  :hook (corfu-mode . corfu-echo-mode))

(use-package corfu-popupinfo
  :load-path "packages/corfu/extensions/"
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.4 . 0.2)))

;; (progn
;;   (eval-and-compile (add-to-list 'load-path "/Users/dn/.emacs.d/packages/cape/"))
;;   (defvar use-package--warning136
;;     #'(lambda (keyword err)
;; 	(let ((msg (format "%s/%s: %s" 'cape keyword (error-message-string err))))
;; 	  (display-warning 'use-package msg :error))))
;;   (condition-case-unless-debug err
;;       (progn
;; 	(unless (fboundp 'cape-file) (autoload #'cape-file "cape" nil t))
;; 	(unless (fboundp 'cape-elisp-block) (autoload #'cape-elisp-block "cape" nil t)))
;;     (error (funcall use-package--warning136 :catch err))))
;; (progn
;;   (defvar use-package--warning135
;;     #'(lambda (keyword err)
;; 	(let ((msg (format "%s/%s: %s" 'cape-keyword keyword (error-message-string err))))
;; 	  (display-warning 'use-package msg :error))))
;;   (condition-case-unless-debug err
;;       (unless (fboundp 'cape-keyword) (autoload #'cape-keyword "cape-keyword" nil t))
;;     (error (funcall use-package--warning135 :catch err))))

(add-to-list 'load-path "~/.emacs.d/packages/cape")
(autoload #'cape-elisp-block "cape" nil t)
(autoload #'cape-file "cape" nil t)
(autoload #'cape-keyword "cape-keyword" nil t)
(add-hook 'completion-at-point-functions #'cape-file)
(add-hook 'completion-at-point-functions #'cape-elisp-block)
(add-hook 'completion-at-point-functions #'cape-keyword)

(use-package stillness-mode
  :load-path "packages/stillness-mode.el/"
  :hook (minibuffer-mode . stillness-mode))

(provide 'init-completion)
;;; init-completion.el ends here
