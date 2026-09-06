;; -*- lexical-binding: t; -*-

;; https://emacs-china.org/t/macos-save-silently-t/24086
(setq inhibit-message-regexps '("^Saving" "^Wrote"))
(setq set-message-functions '(inhibit-message))

(setq enable-recursive-minibuffers t
      read-minibuffer-restore-windows nil
      minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)
      minibuffer-follows-selected-frame nil
      minibuffer-completion-auto-choose t
      minibuffer-completion-confirm 'confirm
      tab-always-indent 'complete
      tab-first-completion 'word-or-paren-or-punct
      completion-cycle-threshold 2
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
(add-hook 'icomplete-minibuffer-setup-hook (lambda ()
					     (setq-local completion-styles '(basic substring initials orderless))))

;; 行内灰色幽灵补全，代替 Corfu。
(add-hook 'prog-mode-hook #'completion-preview-mode)
(add-hook 'org-mode-hook #'completion-preview-mode)
(add-hook 'comint-mode-hook #'completion-preview-mode)

(with-eval-after-load 'completion-preview
  (keymap-set completion-preview-active-mode-map "TAB" #'completion-preview-insert)
  (keymap-set completion-preview-active-mode-map "M-RET" #'completion-preview-complete)
  (keymap-set completion-preview-active-mode-map "C-n" #'completion-preview-next-candidate)
  (keymap-set completion-preview-active-mode-map "C-p" #'completion-preview-prev-candidate))

(with-eval-after-load 'orderless
  (setq orderless-matching-styles '(orderless-prefixes orderless-regexp))
  (setq completion-styles '(basic substring initials orderless))
  (setq completion-pcm-leading-wildcard t)
  (setq completions-format 'vertical)
  (setq completion-category-defaults nil)
  (setq completion-category-overrides
        '((file (styles . (basic partial-completion orderless)))
          (bookmark (styles . (basic substring)))
          (library (styles . (basic substring)))
          (embark-keybinding (styles . (basic substring)))
          (imenu (styles . (basic substring orderless)))
          (consult-location (styles . (basic substring orderless)))
          (kill-ring (styles . (emacs22 orderless)))
          (eglot (styles . (emacs22 substring orderless))))))

(defun my/ensure-orderless-before-completion ()
  "Load Orderless if the minibuffer wins the race with idle preloading."
  (require 'orderless)
  (remove-hook 'minibuffer-setup-hook
               #'my/ensure-orderless-before-completion))

(add-hook 'minibuffer-setup-hook #'my/ensure-orderless-before-completion)

(add-hook 'fido-mode-hook #'marginalia-mode)

;; consult
(advice-add 'consult-recent-file :before
            (lambda (&rest _)
              (unless recentf-mode
                (recentf-mode 1))))

(global-set-key [remap apropos] #'consult-apropos)
(global-set-key [remap bookmark-jump] #'consult-bookmark)
(global-set-key [remap goto-line] #'consult-line)
(global-set-key [remap locate] #'consult-locate)
(global-set-key [remap load-theme] #'consult-theme)
(global-set-key [remap man] #'consult-man)
(global-set-key [remap recentf-open-files] #'consult-recent-file)
(global-set-key [remap switch-to-buffer-other-window] #'consult-buffer-other-window)
(global-set-key [remap switch-to-buffer-other-frame] #'consult-buffer-other-frame)
(global-set-key [remap yank-pop] #'consult-yank-pop)
(global-set-key [remap imenu] #'consult-imenu)
(global-set-key [remap rg] #'consult-ripgrep)

(with-eval-after-load 'consult
  (setq consult-narrow-key "<")
  (setq consult-preview-key "M-."))

(with-eval-after-load 'minibuffer
  (define-key minibuffer-mode-map (kbd "C-c C-r") #'consult-history))

(with-eval-after-load 'org
  (define-key org-mode-map "M-g h" #'consult-org-heading))

;; consult-dir
(global-set-key [remap list-directory] #'consult-dir)

(global-set-key [remap describe-bindings] #'embark-bindings)
(global-set-key (kbd "C-;") #'embark-act)
(global-set-key (kbd "M-.") #'embark-dwim)

(with-eval-after-load 'minibuffer
  (define-key minibuffer-local-map (kbd "C-;") #'embark-act)
  (define-key minibuffer-local-map (kbd "C-c C-e") #'embark-export)
  (define-key minibuffer-local-map (kbd "C-c C-l") #'embark-collect))


(provide 'init-completion)
