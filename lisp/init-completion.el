;; init-completion.el --- Minibuffer and completion. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(setq completion-category-overrides '((file (styles basic partial-completion))))
(setq read-file-name-completion-ignore-case t)

(setq-local completion-in-region-function
            (lambda (&rest args)
              (apply (if vertico-mode
                         #'consult-completion-in-region
                       #'completion--in-region)
                     args)))

(setq tab-always-indent 'complete)

(use-package orderless
  :config
  (setq completion-styles '(orderless partial-completion)))

(use-package vertico
  ;; :load-path "~/.emacs.d/packages/vertico"
  :hook (after-init . vertico-mode)
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous))
  :config
  (setq vertico-count 15)
  (setq vertico-resize nil)
  (setq vertico-cycle t))

(use-package vertico-directory
  :bind (:map vertico-map
              ("C-<backspace>" . vertico-directory-up)))

(use-package vertico-indexed
  :hook (vertico-mode . vertico-indexed-mode))

(use-package marginalia
  :hook ((minibuffer-setup . marginalia-mode)))

(use-package embark
  :commands embark-open-externally
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         (:map vertico-map
               ("C-c C-o" . embark-export)
               ("C-c C-c" . embark-act)))
  :config
  (add-to-list 'display-buffer-alist
               '("\\*Embark Actions\\*"
                 (display-buffer-in-new-tab)
                 (side . right)
                 (width . 0.3)))
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after embark consult)

(defun consult-outline-insert-heading (target)
  (let* ((marker (plist-get
                  (text-properties-at 0 target)
                  'consult--candidate))
         (headline-name (cadr (split-string (org-no-properties target) "* "))))
    (org-insert-link nil headline-name nil)))

(with-eval-after-load 'embark
  (define-key embark-general-map (kbd "h") #'consult-outline-insert-heading))

(use-package consult
  :commands consult-outline consult-find
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (("C-c f r" . consult-recent-file)
         ([remap apropos] . consult-apropos)
         ([remap bookmark-jump] . consult-bookmark)
         ([remap goto-line] . consult-line)
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

(use-package consult-imenu
  :bind (([remap imenu] . consult-imenu)))

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         (:map minibuffer-mode-map
               ("C-x C-d" . consult-dir)
               ("C-x C-j" . consult-dir-jump-file))))

(use-package corfu
  :hook (after-init . global-corfu-mode)
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
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package corfu-echo
  :hook (corfu-mode . corfu-echo-mode))

(use-package corfu-popupinfo
  :hook (corfu-mode . corfu-popupinfo-mode))

(use-package corfu-prescient
  :hook (corfu-mode . corfu-prescient-mode))

(use-package kind-icon
  :commands kind-icon-margin-formatter
  :config
  (setq kind-icon-use-icons nil)
  ;; (setq kind-icon-default-face 'corfu-default)
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
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
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

(provide 'init-completion)
;;; init-git.el ends here.
