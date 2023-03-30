;; init-dired.el --- File manager. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package dired
  :bind (("C-x d" . dired)
         (:map dired-mode-map
               ("C-c l" . org-store-link)))
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq insert-directory-program "/opt/homebrew/bin/gls")
  (setq dired-use-ls-dired t)
  (setq dired-dwim-target t)
  (setq dired-auto-revert-buffer #'dired-buffer-stale-p)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'top)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  (setq dired-auto-revert-buffer t)
  (add-to-list 'display-buffer-alist '((or (derived-mode . dired-mode)
                                           (derived-mode . dirvish-mode))
                                       (display-buffer-in-tab)
                                       (tab-name . "Dired")
                                       (tab-group . "Dired"))))

(defun my/eww-html-file ()
  (interactive)
  (let* ((file (dired-get-filename)))
    (eww (concat "file://" file))))

(define-key dired-mode-map (kbd "C-c e") 'my/eww-html-file)

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (:map dired-mode-map
              ("s-." . dired-hide-dotfiles-mode)))

(use-package image-dired
  :bind ("C-c d" . image-dired)
  :config
  (setq image-dired-dir (expand-file-name "cache/image-dired" user-emacs-directory)))

(provide 'init-dired)
;;; init-dired.el ends here.
