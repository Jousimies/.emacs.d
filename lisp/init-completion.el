;; -*- lexical-binding: t; -*-

(setopt enable-recursive-minibuffers t)
(setopt read-minibuffer-restore-windows nil
  	minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))

(setopt minibuffer-completion-auto-choose t
	minibuffer-completion-confirm 'confirm)
(setopt tab-always-indent 'complete
	tab-first-completion 'word-or-paren-or-punct)

(setup minibuffer
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
  (add-hook 'minibuffer-setup-hook (lambda ()
  				     (setq-local truncate-lines t)))
  (keymap-set minibuffer-local-completion-map "C-n" #'icomplete-forward-completions)
  (keymap-set minibuffer-local-completion-map "C-p" #'icomplete-backward-completions))

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

(add-hook 'emacs-startup-hook 'fido-vertical-mode)

;; 行内灰色幽灵补全，代替 Corfu。
(add-hook 'prog-mode-hook #'completion-preview-mode)
(add-hook 'org-mode-hook #'completion-preview-mode)
(add-hook 'comint-mode-hook #'completion-preview-mode)

(with-eval-after-load 'completion-preview
  (keymap-set completion-preview-active-mode-map "TAB" #'completion-preview-insert)
  (keymap-set completion-preview-active-mode-map "C-n" #'completion-preview-next-candidate)
  (keymap-set completion-preview-active-mode-map "C-p" #'completion-preview-prev-candidate))

(provide 'init-completion)
