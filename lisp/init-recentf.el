;;; init-recentf.el --- Recentf. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
(add-hook 'after-init-hook 'recentf-mode)
(run-with-idle-timer 1 nil (lambda ()
                             (recentf-cleanup)))
(with-eval-after-load 'recentf
  (setq recentf-max-saved-items 1000)
  (setq recentf-exclude nil))

(general-define-key
 :states '(normal visual emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "ff" '(find-file :wk "Find file")
 "fr" '(recentf-open-files :wk "Recent files"))

;;Undo-fu, undo-fu-session and vundo
(require-package 'undo-fu)
(require-package 'undo-fu-session)
(require-package 'vundo)

(add-hook 'after-init-hook 'global-undo-fu-session-mode)

(with-eval-after-load 'vundo
  (setq vundo-glyph-alist vundo-unicode-symbols))

(global-set-key (kbd "C-x u") 'vundo)

(when (maybe-require-package 'hungry-delete)
  (setq-default hungry-delete-chars-to-skip " \t\n\r\f\v")
  (add-hook 'text-mode-hook 'hungry-delete-mode)
  (add-hook 'org-mode-hook 'hungry-delete-mode))

;; Auto save
(require 'auto-save)
(auto-save-enable)
(setq auto-save-silent t)
(setq auto-save-delete-trailing-whitespace t)

(defun my/emacs-config ()
  "My literate Emacs configuration."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(general-define-key
 :keymaps '(normal visual emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "f" '(:ignore t :wk "Files")
 "fi" '(my/emacs-config :wk "Emacs configuration"))


(defun switch-to-message ()
  "Quick switch to `*Message*' buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun switch-to-scratch ()
  "Quick switch to `*Scratch*' buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(general-define-key
 :keymaps '(normal visual emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "b" '(:ignore t :wk "Buffer/Bibtex")
 "bb" '(switch-to-buffer :wk "Switch buffer")
 "be" '(eval-buffer :wk "Eval buffer")
 "bs" '(switch-to-scratch :wk "Swtich to scratch")
 "bm" '(switch-to-message :wk "Swtich to message"))
(provide 'init-recentf)
;;; init-recentf.el ends here
