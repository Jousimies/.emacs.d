(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'modus-operandi t))
    ('dark (load-theme 'modus-vivendi t))))

(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

(provide 'init-theme)
;;; init-theme.el ends here.
