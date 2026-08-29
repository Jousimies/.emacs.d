;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(unless (fboundp 'decf)
  (defalias 'decf 'cl-decf)
  (defalias 'incf 'cl-incf))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-util)
(my/add-used-packages-to-load-path)

(when init-file-debug
  (require 'benchmark-init-loaddefs)
  (benchmark-init/activate)
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(require 'init-idle)
(require 'init-vars)

(require 'init-font)
(require 'init-modeline)

(my/idle-loader-add '(require 'init-ui))

;; Require configurations
(require 'init-builtin)
(require 'init-modal)
(require 'init-edit)
(my/idle-loader-add '(require 'init-dired))
(require 'init-completion)
(require 'init-buffer)

(require 'init-org)
(require 'init-note)
(require 'init-bib)
;; (require 'init-latex)
(require 'init-reader)

(require 'init-gtd)

(require 'init-prog)
(my/idle-loader-add '(require 'init-git))
(require 'init-ai)
(require 'init-misc)
(require 'init-keys)

;; Custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Show startup times
(add-hook 'window-setup-hook
          (lambda ()
            (message "window-setup: %.3fs, after-init: %.3fs"
                     (float-time (time-subtract nil before-init-time))
                     (float-time (time-subtract after-init-time before-init-time)))))
(put 'downcase-region 'disabled nil)
