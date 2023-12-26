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

(add-hook 'minibuffer-mode-hook #'minibuffer-electric-default-mode)
(add-hook 'minibuffer-mode-hook #'cursor-intangible-mode)

(setopt tab-always-indent 'complete
		completions-detailed t
        completions-format 'one-column
        completion-auto-select t
        completion-ignore-case t
        minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)
        read-buffer-completion-ignore-case t
        completion-show-inline-help nil
        completions-max-height 50
        completion-show-help nil
        completion-auto-wrap nil
        completions-header-format (propertize "%s candidates:\n" 'face 'font-lock-comment-face)
        completions-highlight-face 'completions-highlight)

(keymap-set minibuffer-mode-map "C-r" #'minibuffer-complete-history)

;; use `M-j' call `icomplete-fido-exit' to exit minibuffer completion.
(add-hook 'on-first-input-hook #'icomplete-mode)

(use-package orderless
  :load-path "~/.emacs.d/packages/orderless/"
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

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
         ("C-c f a" . consult-find)
         :map minibuffer-mode-map
         ("C-r" . consult-history))
  :config
  (setq consult-preview-key nil))

(use-package consult-imenu
  :bind ([remap imenu] . consult-imenu))

(use-package marginalia
  :load-path "packages/marginalia/"
  :hook (minibuffer-setup . marginalia-mode))

(use-package embark
  :load-path "packages/embark/"
  :bind (([remap describe-bindings] . embark-bindings)
         ("C-;" . embark-act)
         ("M-." . embark-dwim)
         ("C-c f o" . embark-open-externally)
         (:map minibuffer-local-map
               ("C-;" . embark-act)
               ("C-c C-e" . embark-export)
               ("C-c C-l" . embark-collect))))

(use-package corfu
  :load-path "packages/corfu/"
  :hook (on-first-buffer . global-corfu-mode)
  :config
  (setopt corfu-cycle t
		  corfu-auto t
		  corfu-auto-prefix 1
		  corfu-auto-delay 0.0
		  corfu-preselect 'valid)

  (setq-default corfu-quit-no-match 'separator)
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package corfu-echo
  :load-path "packages/corfu/extensions/"
  :hook (corfu-mode . corfu-echo-mode))

(use-package corfu-popupinfo
  :load-path "packages/corfu/extensions/"
  :hook (corfu-mode . corfu-popupinfo-mode))

(use-package kind-icon
  :load-path "packages/kind-icon/"
  :commands kind-icon-margin-formatter
  :config
  (setq kind-icon-use-icons nil)
  (setq kind-icon-default-face 'corfu-default)
  (setq kind-icon-mapping
        `(
          (array ,(nerd-icons-codicon "nf-cod-symbol_array") :face font-lock-type-face)
          (boolean ,(nerd-icons-codicon "nf-cod-symbol_boolean") :face font-lock-builtin-face)
          (class ,(nerd-icons-codicon "nf-cod-symbol_class") :face font-lock-type-face)
          (color ,(nerd-icons-codicon "nf-cod-symbol_color") :face success)
          (command ,(nerd-icons-codicon "nf-cod-terminal") :face default)
          (constant ,(nerd-icons-codicon "nf-cod-symbol_constant") :face font-lock-constant-face)
          (constructor ,(nerd-icons-codicon "nf-cod-triangle_right") :face font-lock-function-name-face)
          (enummember ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
          (enum-member ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
          (enum ,(nerd-icons-codicon "nf-cod-symbol_enum") :face font-lock-builtin-face)
          (event ,(nerd-icons-codicon "nf-cod-symbol_event") :face font-lock-warning-face)
          (field ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-variable-name-face)
          (file ,(nerd-icons-codicon "nf-cod-symbol_file") :face font-lock-string-face)
          (folder ,(nerd-icons-codicon "nf-cod-folder") :face font-lock-doc-face)
          (interface ,(nerd-icons-codicon "nf-cod-symbol_interface") :face font-lock-type-face)
          (keyword ,(nerd-icons-codicon "nf-cod-symbol_keyword") :face font-lock-keyword-face)
          (macro ,(nerd-icons-codicon "nf-cod-symbol_misc") :face font-lock-keyword-face)
          (magic ,(nerd-icons-codicon "nf-cod-wand") :face font-lock-builtin-face)
          (method ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
          (function ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
          (module ,(nerd-icons-codicon "nf-cod-file_submodule") :face font-lock-preprocessor-face)
          (numeric ,(nerd-icons-codicon "nf-cod-symbol_numeric") :face font-lock-builtin-face)
          (operator ,(nerd-icons-codicon "nf-cod-symbol_operator") :face font-lock-comment-delimiter-face)
          (param ,(nerd-icons-codicon "nf-cod-symbol_parameter") :face default)
          (property ,(nerd-icons-codicon "nf-cod-symbol_property") :face font-lock-variable-name-face)
          (reference ,(nerd-icons-codicon "nf-cod-references") :face font-lock-variable-name-face)
          (snippet ,(nerd-icons-codicon "nf-cod-symbol_snippet") :face font-lock-string-face)
          (string ,(nerd-icons-codicon "nf-cod-symbol_string") :face font-lock-string-face)
          (struct ,(nerd-icons-codicon "nf-cod-symbol_structure") :face font-lock-variable-name-face)
          (text ,(nerd-icons-codicon "nf-cod-text_size") :face font-lock-doc-face)
          (typeparameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
          (type-parameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
          (unit ,(nerd-icons-codicon "nf-cod-symbol_ruler") :face font-lock-constant-face)
          (value ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-builtin-face)
          (variable ,(nerd-icons-codicon "nf-cod-symbol_variable") :face font-lock-variable-name-face)
          (t ,(nerd-icons-codicon "nf-cod-code") :face font-lock-warning-face))))

(use-package cape
  :load-path "packages/cape/"
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p w" . cape-dict)
         ("C-c p l" . cape-line)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(use-package which-key
  :load-path "packages/emacs-which-key/"
  :hook (on-first-input . which-key-mode)
  :config
  (define-key help-map "\C-h" 'which-key-C-h-dispatch)
  (setq which-key-popup-type 'minibuffer)
  (setq which-key-sort-order #'which-key-prefix-then-key-order)
  ;; (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 0)
  (setq which-key-idle-secondary-delay 0.05))


(provide 'init-completion)
;;; init-completion.el ends here
