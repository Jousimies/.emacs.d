;; -*- lexical-binding: t; -*-

(setopt enable-recursive-minibuffers t)
(setopt read-minibuffer-restore-windows nil
  	minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
(setopt minibuffer-follows-selected-frame nil)

(setopt minibuffer-completion-auto-choose t
	minibuffer-completion-confirm 'confirm)
(setopt tab-always-indent 'complete
	tab-first-completion 'word-or-paren-or-punct)

(setopt completion-cycle-threshold 2
	completions-detailed t
	completions-format 'one-column
	completion-auto-select t
	completion-ignore-case t
	completion-show-inline-help nil
	completions-max-height 50
	completion-show-help nil
	completion-auto-wrap nil
	completions-header-format (propertize "%s candidates:\n" 'face 'font-lock-comment-face)
	completions-highlight-face 'completions-highlight)

(keymap-set minibuffer-mode-map "C-r" #'minibuffer-complete-history)
(add-hook 'minibuffer-mode-hook #'minibuffer-electric-default-mode)
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(add-hook 'minibuffer-setup-hook (lambda () (setq-local truncate-lines t)))
(add-hook 'completion-list-mode-hook (lambda () (setq-local truncate-lines t)))

(keymap-set minibuffer-local-completion-map "C-n" #'icomplete-forward-completions)
(keymap-set minibuffer-local-completion-map "C-p" #'icomplete-backward-completions)


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

(add-hook 'on-first-buffer-hook 'fido-vertical-mode)

;; 行内灰色幽灵补全，代替 Corfu。
(add-hook 'prog-mode-hook #'completion-preview-mode)
(add-hook 'org-mode-hook #'completion-preview-mode)
(add-hook 'comint-mode-hook #'completion-preview-mode)

(with-eval-after-load 'completion-preview
  (keymap-set completion-preview-active-mode-map "TAB" #'completion-preview-insert)
  (keymap-set completion-preview-active-mode-map "C-n" #'completion-preview-next-candidate)
  (keymap-set completion-preview-active-mode-map "C-p" #'completion-preview-prev-candidate))

(use-package orderless
  :custom
  (orderless-matching-styles '(orderless-prefixes orderless-regexp))
  (completion-styles '(basic substring initials orderless))
  ;; (completion-styles '(flex))
  (completion-pcm-leading-wildcard t)
  (completions-format 'vertical)
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

(use-package marginalia
  :hook (minibuffer-setup . marginalia-mode))

(use-package consult
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
         :map minibuffer-mode-map
         ("C-r" . consult-history))
  :custom
  (consult-narrow-key "<")
  (consult-preview-key "M-."))

;; Bind org-mode keys safely
(with-eval-after-load 'org
  (require 'consult-org)
  (define-key org-mode-map (kbd "M-g h") #'consult-org-heading))

(use-package embark
  :bind (([remap describe-bindings] . embark-bindings)
         ("C-;" . embark-act)
         ("M-." . embark-dwim)
         (:map minibuffer-local-map
               ("C-;" . embark-act)
               ("C-c C-e" . embark-export)
               ("C-c C-l" . embark-collect))))

(use-package embark-consult
  :after consult embark)


(provide 'init-completion)
