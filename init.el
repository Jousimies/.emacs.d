;; -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-elpaca)
(require 'init-vars)
(require 'init-util)

;; Require configurations
(require 'init-builtin)

(require 'init-modal)
(require 'init-edit)
(require 'init-dired)
(require 'init-ui)
(require 'init-modeline)
(require 'init-completion)
(require 'init-buffer)

(require 'init-org)
(require 'init-note)
(require 'init-bib)
(require 'init-latex)
(require 'init-reader)

(require 'init-gtd)

(require 'init-prog)
(require 'init-git)

(require 'init-misc)

;; Custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Show startup times
(add-hook 'window-setup-hook
          (lambda ()
            (message "window-setup: %.3fs, after-init: %.3fs"
                     (float-time (time-subtract nil before-init-time))
                     (float-time (time-subtract after-init-time before-init-time)))))
