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

(add-hook 'minibuffer-mode-hook #'minibuffer-electric-default-mode)
(add-hook 'minibuffer-mode-hook #'cursor-intangible-mode)
(add-hook 'completion-list-mode-hook (lambda ()
									   (setq-local truncate-lines t)))
(add-hook 'minibuffer-setup-hook
          (lambda () (setq-local truncate-lines t)))

(setopt tab-always-indent 'complete
		tab-first-completion 'word-or-paren-or-punct
		completions-detailed t
        completions-format 'one-column
        completion-auto-select t
        completion-ignore-case t
        minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)
        completion-show-inline-help nil
        completions-max-height 50
        completion-show-help nil
        completion-auto-wrap nil
        completions-header-format (propertize "%s candidates:\n" 'face 'font-lock-comment-face)
        completions-highlight-face 'completions-highlight)

(keymap-set minibuffer-mode-map "C-r" #'minibuffer-complete-history)

;; (use-package nerd-icons-completion
;;   :load-path "packages/nerd-icons-completion/"
;;   :hook (minibuffer-setup . nerd-icons-completion-mode))

;; use `M-j' call `icomplete-fido-exit' to exit minibuffer completion.

;; re-use vertico-mode instead of `icomplete-fido-mode'.
;; Due to icomplete has compatible problem with citar, a references manager.
;; use `M-RET' to exit minibuffer input.

;; (use-package icomplete
;;   :hook (on-first-input . icomplete-mode)
;;   :custom
;;   (icomplete-in-buffer t))

;; (vertico-mode 0)
(use-package vertico
  :load-path "packages/vertico/"
  :hook (on-first-input . vertico-mode))

(use-package vertico-directory
  :load-path "packages/vertico/extensions/"
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :bind (:map vertico-map
		 ("C-DEL" . vertico-directory-up)))

(use-package orderless
  :load-path "packages/orderless/"
  :custom
  (orderless-matching-styles '(orderless-prefixes orderless-regexp))
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; (add-to-list 'load-path "~/.emacs.d/packages/orderless/")
;; (require 'orderless)
;; (setq orderless-matching-styles '(orderless-prefixes orderless-regexp))
;; (with-eval-after-load 'orderless
;;   (setq completion-styles '(orderless basic))
;;   (setq completion-category-overrides '((file (styles basic partial-completion)))))
;; (setq completion-styles '(basic substring initials flex orderless))
;; (setq completion-category-defaults nil)
;; (setq completion-category-overrides
;;         '((file (styles . (basic partial-completion orderless)))
;;           (bookmark (styles . (basic substring)))
;;           (library (styles . (basic substring)))
;;           (embark-keybinding (styles . (basic substring)))
;;           (imenu (styles . (basic substring orderless)))
;;           (consult-location (styles . (basic substring orderless)))
;;           (kill-ring (styles . (emacs22 orderless)))
;;           (eglot (styles . (emacs22 substring orderless)))))

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
		 ("M-g l" . consult-goto-line)
         :map minibuffer-mode-map
         ("C-r" . consult-history))
  :config
  (setq consult-preview-key "C-."))
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
;;   (completion-preview-ignore-case t)
;;  (completion-preview-minimum-symbol-length 1))

(use-package corfu
  :load-path "packages/corfu/"
  :hook (on-first-buffer . global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.0)
  (corfu-preselect 'valid)
  (corfu-quit-no-match t))

(use-package nerd-icons-corfu
  :load-path "packages/nerd-icons-corfu/")
(with-eval-after-load 'corfu
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package corfu-echo
  :load-path "packages/corfu/extensions/"
  :hook (corfu-mode . corfu-echo-mode))

(use-package corfu-popupinfo
  :load-path "packages/corfu/extensions/"
  :hook (corfu-mode . corfu-popupinfo-mode))

(use-package cape
  :load-path "packages/cape/"
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dict))

(global-set-key (kbd "C-c p") #'cape-prefix-map)

(use-package which-key
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 0.1)
  :config
  (which-key-add-key-based-replacements
	"C-c p" "cape"
	"C-x t" "tab"
	"C-c &" "yasnippet"))


(provide 'init-completion)
;;; init-completion.el ends here
