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
(setopt enable-recursive-minibuffers t)
(setopt read-minibuffer-restore-windows nil)

(setopt tab-always-indent 'complete
		tab-first-completion 'word-or-paren-or-punct)

(setq-default tab-width 4
			  indent-tabs-mode nil)

(use-package simple
  :ensure nil
  :custom
  (completion-auto-select t)
  (completion-show-help nil)
  (completion-auto-wrap nil))

(keymap-set minibuffer-mode-map "C-r" #'minibuffer-complete-history)

;; https://emacs-china.org/t/macos-save-silently-t/24086
(setq inhibit-message-regexps '("^Saving" "^Wrote"))
(setq set-message-functions '(inhibit-message))

(use-package minibuffer
  :ensure nil
  :hook ((minibuffer-mode . minibuffer-electric-default-mode)
		 (minibuffer-mode . cursor-intangible-mode)
		 (minibuffer-mode . minibuffer-depth-indicate-mode))
  :custom
  (completion-styles '(basic substring initials flex orderless))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles . (basic partial-completion orderless)))
     (bookmark (styles . (basic substring)))
     (library (styles . (basic substring)))
     (embark-keybinding (styles . (basic substring)))
     (imenu (styles . (basic substring orderless)))
     (consult-location (styles . (basic substring orderless)))
     (kill-ring (styles . (emacs22 orderless)))
     (eglot (styles . (emacs22 substring orderless)))))
  (minibuffer-completion-auto-choose t)
  (minibuffer-visible-completions t)
  (completions-sort 'historical)
  (completion-show-inline-help nil)
  (completions-max-height 50)
  (completions-detailed t)
  (completions-format 'one-column)
  (read-file-name-completion-ignore-case t)
  (completions-header-format (propertize "%s candidates:\n" 'face 'font-lock-comment-face))
  (completions-highlight-face 'completions-highlight))

(use-package orderless
  :custom
  (orderless-matching-styles '(orderless-prefixes orderless-regexp))
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; use `M-j' call `icomplete-fido-exit' to exit minibuffer completion.
;; use `M-RET' to exit minibuffer input.
;; fido-mode do not work well with rime-regexp
(use-package icomplete
  :ensure nil
  :hook ((on-first-input . icomplete-mode)
         (completion-list-mode . (lambda ()
                                   (setq-local truncate-lines t)))))
;; (use-package vertico
;;   :hook ((on-first-buffer . vertico-mode)
;; 		 (rfn-eshadow-update-overlay . vertico-directory-tidy)
;;          (vertico-mode . vertico-multiform-mode))
;;   :bind (:map vertico-map
;; 			  ("C-<backspace>" . vertico-directory-up)))

;; (with-eval-after-load 'vertico-multiform
;;   (add-to-list 'vertico-multiform-categories '(embark-keybinding grid)))

(use-package nerd-icons-completion
  :hook (minibuffer-setup . nerd-icons-completion-mode))

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

(use-package consult
  ;; :hook (completion-list-mode . consult-preview-at-point-mode)
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
		 ([remap imenu] . consult-imenu)
		 ("M-g G" . consult-goto-line)
         :map minibuffer-mode-map
		 ("C-r" . consult-history))
  :custom
  (consult-preview-key 'any))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-g h") #'consult-org-heading))

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

;;;###autoload
(defun my/consult-find-attach ()
  (interactive)
  (let* ((dir (expand-file-name "attach" my-galaxy)))
	(consult-find dir)))

(use-package marginalia
  :hook (minibuffer-setup . marginalia-mode))

(use-package embark
  :bind (([remap describe-bindings] . embark-bindings)
         ("C-;" . embark-act)
         ("M-." . embark-dwim)
         :map minibuffer-local-map
         ("C-;" . embark-act)
         ("C-c C-e" . embark-export)
         ("C-c C-l" . embark-collect))
  :custom
  (prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after embark)

(use-package corfu
  :hook ((on-first-buffer . global-corfu-mode)
		 (corfu-mode . corfu-echo-mode)
         (corfu-mode . corfu-history-mode)
		 (corfu-mode . corfu-popupinfo-mode))
  :bind (:map corfu-map
              ("SPC" . corfu-insert-separator)
              ("RET" . nil))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.0)
  (corfu-preselect 'valid)
  (corfu-quit-no-match t))

(use-package nerd-icons-corfu)

(with-eval-after-load 'corfu
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :bind ("C-c p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dict))

(use-package which-key
  :hook (on-first-input . which-key-mode)
  :custom
  (which-key-idle-delay 0.1))


(provide 'init-completion)
;;; init-completion.el ends here
