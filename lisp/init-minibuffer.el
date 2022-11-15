;;; init-minibuffer.el --- Config for minibuffer completion. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

(setq-default read-extended-command-predicate #'command-completion-default-include-p)
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

;; https://github.com/Ergus/mini_dotemacs
(add-hook 'minibuffer-setup-hook #'my/unset-gc)
(add-hook 'minibuffer-exit-hook #'my/restore-gc)

(when (maybe-require-package 'vertico)
  (add-hook 'after-init-hook 'vertico-mode)

  (when (maybe-require-package 'vertico-prescient)
    (add-hook 'vertico-mode-hook 'vertico-prescient-mode))

  (when (maybe-require-package 'embark)
    (with-eval-after-load 'vertico
      (define-key vertico-map (kbd "C-c C-o") 'embark-export)
      (define-key vertico-map (kbd "C-c C-c") 'embark-act))

    (setq prefix-help-command #'embark-prefix-help-command)

    (global-set-key (kbd "C-.") 'embark-act)
    (global-set-key (kbd "C-;") 'embark-dwim)

    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none)))))

  (with-eval-after-load 'vertico
    (define-key vertico-map (kbd "C-j") 'vertico-directory-up)
    (setq vertico-cycle t)
    (with-eval-after-load 'consult
      (setq completion-in-region-function
            (lambda (&rest args)
              (apply (if vertico-mode
                         #'consult-completion-in-region
                       #'completion--in-region)
                     args)))))

  (when (maybe-require-package 'consult)
    (add-hook 'completion-list-mode-hook 'consult-preview-at-point-mode)

    (global-set-key [remap apropos] 'consult-apropos)
    (global-set-key [remap bookmark-jump] 'consult-bookmark)

    (global-set-key [remap goto-line] 'consult-goto-line)
    (global-set-key [remap imenu] 'consult-imenu)
    (global-set-key [remap locate] 'consult-locate)
    (global-set-key [remap load-theme] 'consult-theme)
    (global-set-key [remap man] 'consult-man)
    (global-set-key [remap recentf-open-files] 'consult-recent-file)
    (global-set-key [remap switch-to-buffer] 'consult-buffer)
    (global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
    (global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
    (global-set-key [remap yank-pop] 'consult-yank-pop)

    (with-eval-after-load 'evil
      (evil-declare-key 'normal org-mode-map
        "gh" 'consult-outline))

    (when (maybe-require-package 'consult-dir)
      (global-set-key (kbd "C-x C-d") 'consult-dir)
      (with-eval-after-load 'vertico
        (define-key vertico-map (kbd "C-x C-d") 'consult-dir)
        (define-key vertico-map (kbd "C-x C-j") 'consult-dir-jump-file)))))


  (when (maybe-require-package 'marginalia)
    (add-hook 'minibuffer-setup-hook 'marginalia-mode))

(provide 'init-minibuffer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-minibuffer.el ends here
