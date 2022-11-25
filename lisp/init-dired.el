;;; init-dired.el ---  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;;listing directory failed but ‘access-file’ worked.
(setq insert-directory-program "/opt/homebrew/bin/gls")
(setq dired-use-ls-dired t)

(setq dired-listing-switches
      "-l --almost-all --human-readable --group-directories-first --no-group")

(setq dired-auto-revert-buffer t)

(when (maybe-require-package 'dirvish)

  (add-hook 'after-init-hook 'dirvish-override-dired-mode)

  (with-eval-after-load 'dirvish
    (setq dirvish-use-header-line 'global)
    (setq dirvish-mode-line-height doom-modeline-height)
    (setq dirvish-default-layout '(0 0.4 0.6))
    (setq dirvish-header-line-format
          '(:left (path) :right (free-space)))
    (setq dirvish-mode-line-format
          '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
    (add-hook 'dirvish-find-entry-hook
              (lambda (&rest _) (setq-local truncate-lines t)))
    (scroll-bar-mode -1)
    (customize-set-variable 'dirvish-quick-access-entries
                            '(("h" "~/"                          "Home")
                              ("d" "~/Downloads/"                "Downloads")
                              ("D" "~/Documents/"                "Documents")
                              ("p" "~/Pictures/"                 "Pictures")
                              ("n" "~/Nextcloud/"                "Nextcloud"))))
  (with-eval-after-load 'evil-collection
    (evil-collection-define-key 'normal 'dirvish-mode-map
      "q" 'dirvish-quit
      "e" 'xah-show-in-desktop)))

;; dired has default keybinding, C-x d, remap it to dirvish.
(global-set-key [remap dired] 'dirvish)

;; hide dotfiles
(when (maybe-require-package 'dired-hide-dotfiles)
  (with-eval-after-load 'evil-collection
    (evil-collection-define-key 'normal 'dirvish-mode-map
      "." 'dired-hide-dotfiles-mode)))

(provide 'init-dired)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dired.el ends here
