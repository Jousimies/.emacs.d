;;; init-corfu.el --- Interactive completion in buffers. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq tab-always-indent 'complete)

(require-package 'orderless)
(setq completion-styles '(orderless basic))

(setq completion-category-defaults nil
      completion-category-overrides '((file (styles . (partial-completion)))))
(setq completion-cycle-threshold 4)


(when (maybe-require-package 'corfu)
  (setq-default corfu-auto t)

  (with-eval-after-load 'eshell
    (add-hook 'eshell-mode-hook (lambda () (setq-local corfu-auto nil))))

  (setq-default corfu-quit-no-match 'separator)
  (add-hook 'after-init-hook 'global-corfu-mode)

  (when (maybe-require-package 'corfu-doc)
    (with-eval-after-load 'corfu
      (add-hook 'corfu-mode-hook #'corfu-doc-mode)))

  (when (maybe-require-package 'kind-icon)
    (with-eval-after-load 'corfu
      (setq kind-icon-default-face 'corfu-default)
      (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

  (when (maybe-require-package 'corfu-prescient)
    (add-hook 'corfu-mode-hook 'corfu-prescient-mode)
    (setq corfu-prescient-completion-styles '(orderless prescient basci))))

(provide 'init-corfu)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-corfu.el ends here
