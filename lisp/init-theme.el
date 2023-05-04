;; init-theme.el --- Themes. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package nerd-icons
  :config
  (setq nerd-icons-font-family "Hack Nerd Font Mono"))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package modus-themes
  :config
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-italic-constructs t)

  (setq modus-themes-common-palette-overrides
        '(;; mode-line
          (border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)
          (bg-mode-line-active bg-main)
          (fg-mode-line-active fg-main)

          ;; line-number
          (fg-line-number-inactive "gray50")
          (fg-line-number-active red-cooler)
          (bg-line-number-inactive unspecified)
          (bg-line-number-active unspecified)
          ;; link
          (underline-link border)
          (underline-link-visited border)
          (underline-link-symbolic border)

          ;; org agenda
          (date-common cyan)   ; default value (for timestamps and more)
          (date-deadline red-warmer)
          (date-event magenta-warmer)
          (date-holiday blue) ; for M-x calendar
          (date-now yellow-warmer)
          (date-scheduled magenta-cooler)
          (date-weekday cyan-cooler)
          (date-weekend blue-faint)

          ;; org heading
          (fg-heading-1 blue-warmer)
          (fg-heading-2 yellow-cooler)
          (fg-heading-3 cyan-cooler)))

  (setq modus-themes-prompts '(extrabold italic))

  (setq modus-themes-completions
        '((matches . (extrabold))
          (selection . (semibold italic text-also)))))

(defun my/modus-cursor-color (var)
  (cadr (assoc (cadr (assoc 'cursor var)) var)))

(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (progn
              (load-theme 'modus-operandi t)
              (when (boundp 'sis-default-cursor-color)
                (setq sis-default-cursor-color
                      (my/modus-cursor-color modus-operandi-palette)))))
    ('dark (progn
             (load-theme 'modus-vivendi-tinted t)
             (when (boundp 'sis-default-cursor-color)
               (setq sis-default-cursor-color
                     (my/modus-cursor-color modus-vivendi-tinted-palette)))))))

(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

(provide 'init-theme)
;;; init-theme.el ends here.
