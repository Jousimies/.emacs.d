;;; init-utils.el ---  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
(when (maybe-require-package 'helpful)
  (global-set-key [remap describe-function] 'helpful-callable)
  (global-set-key [remap describe-variable] 'helpful-variable)
  (global-set-key [remap describe-key] 'helpful-key))

(when (maybe-require-package 'browse-kill-ring)
  (browse-kill-ring-default-keybindings))

(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


(provide 'init-utils)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
