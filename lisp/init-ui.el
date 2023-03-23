(use-package emacs-nerd-icons
  :config
  (setq emacs-nerd-icons-font-family "Hack Nerd Font Mono"))

(use-package emacs-nerd-icons-dired
  :hook (dired-mode . emacs-nerd-icons-dired-mode))

(define-fringe-bitmap 'right-curly-arrow  [])
(define-fringe-bitmap 'left-curly-arrow  [])

(fringe-mode '(1 . 1))

(use-package hl-line
  :hook ((prog-mode . hl-line-mode)
         (org-mode . hl-line-mode)
         (profiler-report-mode . hl-line-mode)))

(use-package display-line-numbers
  :hook ((prog-mode . display-line-numbers-mode)
         (org-mode . display-line-numbers-mode)
         (LaTeX-mode . display-line-numbers-mode))
  :init
  (setq-default display-line-numbers-widen t)
  :config
  (setq display-line-numbers-type 'relative))

(scroll-bar-mode 0)

(use-package display-fill-column-indicator
  :hook (prog-mode . display-fill-column-indicator-mode)
  :config
  (face-spec-set 'fill-column-indicator
                 '((default :height 0.1))
                 'face-override-spec)
  (setq-default fill-column 90))

(use-package paren
  :hook (text-mode . show-paren-mode)
  :config
  (setq show-paren-style 'parenthesis)
  (setq show-paren-context-when-offscreen 'overlay))

(blink-cursor-mode -1)

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
                        "red"
                      (foreground-color-at-point))))

(add-hook 'post-command-hook 'im-change-cursor-color)

(defun my/mode-line-padding ()
  (let* ((r-length (string-width (format-mode-line global-mode-string))))
    (propertize " "
                'display `(space :align-to (- right ,(+ r-length 1))))))

(add-to-list 'global-mode-string
             '(:eval (propertize
                      (concat
                       "𝚻𝚨𝚩: "
                       ;; (number-to-string (tab-bar--current-tab-index))
                       ;; ": "
                       (alist-get 'group (tab-bar--current-tab))) 'face 'font-lock-constant-face)))

(setq mode-line-end-spaces
      '(""
        global-mode-string))

(setq mode-line-position-column-line-format '(" %l,%c"))

(setq mode-line-percent-position '(-4 "%p"))

(setq-default mode-line-format
              `("%e"
                mode-line-front-space
                (:propertize ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote))
                mode-line-frame-identification
                mode-line-buffer-identification
                "  "
                mode-line-position
                ;; (:eval (propertize " %I " 'face 'font-lock-constant-face))
                "  "
                (vc-mode vc-mode)
                (:eval (when buffer-read-only
                         (concat "  "  (propertize "RO"
                                                   'face 'font-lock-type-face
                                                   'help-echo "Buffer is read-only"))))
                (:eval (my/mode-line-padding))
                mode-line-end-spaces))

(use-package mode-line-bell
  :defer 1
  :config
  (mode-line-bell-mode))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(evil-paste-after
                   evil-paste-pop
                   evil-paste-before
                   evil-delete
                   evil-delete-line))
  (advice-add command :after #'pulse-line))

(use-package color-identifiers-mode
  :hook (text-mode . global-color-identifiers-mode))

(use-package page-break-lines
  :hook (org-mode . page-break-lines-mode))

(use-package prog-mode
  :hook ((prog-mode . prettify-symbols-mode)
         (LaTeX-mode . prettify-symbols-mode))
  :config
  (setq prettify-symbols-alist '(("lambda" . ?λ)
                                 ("function" . ?𝑓))))

(provide 'init-ui)
;;; init-ui.el ends here.
