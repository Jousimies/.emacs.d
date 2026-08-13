;; -*- lexical-binding: t; -*-

;; Package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize t)
(unless package-archive-contents
  (package-refresh-contents))
;; (setq package-check-signature nil)
;; (setq package-quickstart t)

;; Benchmark
(use-package benchmark-init
  :ensure t
  :preface (package-activate 'benchmark-init)
;;   :commands benchmark-init/deactivate
  :hook (after-init . benchmark-init/deactivate))

;; on.el
(use-package on
  :ensure t
  :preface (package-activate 'on))

;; Require configurations
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-vars)
(require 'init-builtin)
(require 'init-edit)
(require 'init-ui)
(require 'init-completion)

(require 'init-org)

;; Custom
(unless custom-file
  (load (setq custom-file (locate-user-emacs-file "custom.el")) t))

;; Show startup times
(add-hook 'window-setup-hook
          (lambda ()
            (message "window-setup: %.3fs, after-init: %.3fs"
                     (float-time (time-subtract nil before-init-time))
                     (float-time (time-subtract after-init-time before-init-time)))))
