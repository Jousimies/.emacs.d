;; init-completion.el --- Minibuffer and completion *- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(setq read-buffer-completion-ignore-case t)
(setq completion-ignore-case t)
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

(use-package simple
  :config
  (setq-default read-extended-command-predicate #'command-completion-default-include-p))

(use-package minibuffer
  :config
  (setq completion-category-overrides '((file (styles basic partial-completion))))
  (setq read-file-name-completion-ignore-case t)

  (setq-local completion-in-region-function
              (lambda (&rest args)
                (apply (if vertico-mode
                           #'consult-completion-in-region
                         #'completion--in-region)
                       args))))

(setq tab-always-indent 'complete)

(use-package orderless
  :config
  (setq completion-styles '(orderless partial-completion)))

(use-package vertico
  :load-path "~/.emacs.d/packages/vertico"
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-cycle t)
  :bind (:map vertico-map
      ("C-j" . vertico-next)
      ("C-k" . vertico-previous)))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
        ("C-u" . vertico-directory-up)))

(use-package marginalia
  :hook ((minibuffer-setup . marginalia-mode)))

(use-package embark
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         (:map vertico-map
               ("C-c C-o" . embark-export)
               ("C-c C-c" . embark-act)))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(my/space-leader-def
  "oe" '(embark-open-externally :wk "Open externally"))

(use-package consult
  :commands consult-outline
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (([remap apropos] . consult-apropos)
         ([remap bookmark-jump] . consult-bookmark)
         ([remap goto-line] . consult-line)
         ([remap imenu] . consult-imenu)
         ([remap locate] . consult-locate)
         ([remap load-theme] . consult-theme)
         ([remap man] . consult-man)
         ([remap recentf-open-files] . consult-recent-file)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap yank-pop] . consult-yank-pop)
         :map minibuffer-mode-map
         ("C-r" . consult-history)))

(with-eval-after-load 'evil
  (evil-declare-key 'normal org-mode-map
    "gh" 'consult-outline)
  (evil-declare-key 'normal LaTeX-mode-map
    "gh" 'consult-outline))

(use-package corfu
  :config
  (setq corfu-cycle t)
  (setq corfu-auto t)
  (setq corfu-auto-prefix 2)
  (setq corfu-auto-delay 0.0)
  (setq corfu-preselect 'valid)

  (setq-default corfu-quit-no-match 'separator)

  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input)
                (eq (current-local-map) read-passwd-map))
      (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  :hook (after-init . global-corfu-mode))

(use-package corfu-echo
  :hook (corfu-mode . corfu-echo-mode))

(use-package corfu-popupinfo
  :hook (corfu-mode . corfu-popupinfo-mode))

(use-package kind-icon
  :commands kind-icon-margin-formatter
  :config
  (setq kind-icon-use-icons t)
  (setq kind-icon-default-face 'corfu-default))

(use-package cape
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
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
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(autoload 'prescient-persist-mode "prescient" "" t)
(add-hook 'after-init-hook 'prescient-persist-mode)
(setq prescient-save-file (expand-file-name "cache/prescient-save.el" user-emacs-directory))
(use-package vertico-prescient
  :hook (vertico-mode . vertico-prescient-mode))

(use-package corfu-prescient
  :hook (corfu-mode . corfu-prescient-mode)
  :config
  (setq vertico-prescient-completion-styles '(orderless prescient partial-completion)))

(provide 'init-completion)
;;; init-git.el ends here.