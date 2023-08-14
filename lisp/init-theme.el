;; init-theme.el --- Themes. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package nerd-icons
  :config
  (setq nerd-icons-font-family "Hack Nerd Font Mono"))

(use-package nerd-icons-completion
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(defun my/modus-cursor-color (var)
  "Get the cursor color from VAR."
  (cdr (assoc 'fg-main var)))

(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (progn
              (load-theme 'modus-operandi t)
              (when (boundp 'sis-default-cursor-color)
                (setq sis-default-cursor-color
                      (my/modus-cursor-color modus-themes-operandi-colors)))))
    ('dark (progn
             (load-theme 'modus-vivendi t)
             (when (boundp 'sis-default-cursor-color)
               (setq sis-default-cursor-color
                     (my/modus-cursor-color modus-themes-vivendi-colors)))))))

(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

(provide 'init-theme)
;;; init-theme.el ends here.
