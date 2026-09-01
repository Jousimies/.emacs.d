;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(unless (fboundp 'decf)
  (defalias 'decf 'cl-decf)
  (defalias 'incf 'cl-incf))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-util)
(my/add-used-packages-to-load-path)

;; Only load with --debug-init
(when init-file-debug
  (require 'init-benchmark))

(require 'init-idle)
(require 'init-vars)

(require 'init-font)
(require 'init-modeline)
(require 'init-ui)

;; Require configurations
(require 'init-builtin)
(require 'init-modal)
(require 'init-edit)
(require 'init-dired)
(require 'init-completion)
(require 'init-buffer)

;; PKM with Org-mode
(require 'init-org)
(require 'init-note)
(require 'init-bib)
(require 'init-latex)
(require 'init-reader)
(require 'init-gtd)

;; Programming
(require 'init-prog)
(require 'init-git)
(require 'init-ai)

;; Misc
(require 'init-misc)

;; Keybindings
(require 'init-keys)

;; Custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Show startup times
(add-hook 'window-setup-hook
          (lambda ()
            (message "window-setup: %.3fs, after-init: %.3fs"
                     (float-time (time-subtract nil before-init-time))
                     (float-time (time-subtract after-init-time before-init-time)))))
