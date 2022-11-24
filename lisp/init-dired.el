;;; init-dired.el ---  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
(setq insert-directory-program "/opt/homebrew/bin/gls")
(setq-default dired-use-ls-dired nil)

(setq-default dired-auto-revert-buffer t)

(when (maybe-require-package 'dirvish)

  (add-hook 'after-init-hook 'dirvish-override-dired-mode)

  (with-eval-after-load 'dirvish
    (setq dirvish-use-header-line 'global)
    (setq dirvish-header-line-format
          '(:left (path) :right (free-space))
          dirvish-mode-line-format
          '(:left (sort file-time " " file-size symlink) :right (omit yank index)))

    (customize-set-variable 'dirvish-quick-access-entries
                            '(("h" "~/"                          "Home")
                              ("d" "~/Downloads/"                "Downloads")
                              ("n" "~/Nextcloud/"                "Nextcloud"))))

  (with-eval-after-load 'evil-collection
    (evil-collection-define-key 'normal 'dirvish-mode-map (kbd "q") 'dirvish-quit)))

(global-set-key [remap dired] 'dirvish)


(provide 'init-dired)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dired.el ends here
