;;; init-completion.el --- Interactive completion in buffers. -*- lexical-binding: t no-byte-compile: t-*-
;;; Commentary:
;;; Code:

(setq tab-always-indent 'complete)

(require-package 'orderless)
(setq completion-styles '(orderless partial-completion))

(setq completion-category-defaults nil
      completion-category-overrides '((file (styles . (partial-completion)))))
(setq completion-cycle-threshold 4)


;; (when (maybe-require-package 'corfu)
;;   (setq-default corfu-auto t)

;;   (with-eval-after-load 'eshell
;;     (add-hook 'eshell-mode-hook (lambda () (setq-local corfu-auto nil))))

;;   (setq-default corfu-quit-no-match 'separator)
;;   (add-hook 'after-init-hook 'global-corfu-mode)

;;   (when (maybe-require-package 'corfu-doc)
;;     (with-eval-after-load 'corfu
;;       (add-hook 'corfu-mode-hook #'corfu-doc-mode)))

;;   (when (maybe-require-package 'kind-icon)
;;     (with-eval-after-load 'corfu
;;       (setq kind-icon-default-face 'corfu-default)
;;       (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

;;   (when (maybe-require-package 'corfu-prescient)
;;     (add-hook 'corfu-mode-hook 'corfu-prescient-mode)
;;     (setq corfu-prescient-completion-styles '(orderless prescient partial-completion))))

(when (maybe-require-package 'company)
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-require-match nil)
  (setq company-icon-margin 3)
  (setq company-backends '((company-capf :with company-yasnippet)
                           (company-dabbrev-code company-keywords company-files)
                           company-dabbrev))
  (when (maybe-require-package 'company-prescient)
    (add-hook 'company-mode-hook 'company-prescient-mode))

  ;; Default company is good enough.
  ;; (when (maybe-require-package 'company-box)
  ;;   (add-hook 'company-mode-hook 'company-box-mode))

  (when (maybe-require-package 'company-posframe)
    (add-hook 'company-mode-hook 'company-posframe-mode))
  (when (maybe-require-package 'company-quickhelp)
    (add-hook 'company-mode-hook 'company-quickhelp-mode)
    (eval-after-load 'company
      '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))))

(provide 'init-completion)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-completion.el ends here
