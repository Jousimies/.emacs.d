;; -*- lexical-binding: t; -*-

;; Package.el
(require 'package)
;; (setq package-archives '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;                          ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
;;                          ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize t)
(unless package-archive-contents
  (package-refresh-contents))
(setq package-check-signature nil)
;; (setq use-package-verbose t)

;; on.el
(use-package on
  :ensure t
  :preface (package-activate 'on))

(use-package gcmh
  :ensure t
  :preface (package-activate 'gcmh)
  :hook (after-init . gcmh-mode)
  :custom
  (gc-cons-percentage 0.1)
  (gcmh-verbose nil)
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold #x1000000))

(advice-add 'after-focus-change-function :after 'garbage-collect)

;; Server
(run-with-idle-timer 2 nil #'server-start)

;; Require configurations
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-vars)
(require 'init-builtin)
(require 'init-edit)
(require 'init-ui)
(require 'init-modeline)
(require 'init-completion)
(require 'init-buffer)

(require 'init-org)
(require 'init-note)
(require 'init-bib)
(require 'init-reader)

(require 'init-gtd)

(require 'init-prog)
(require 'init-git)

(require 'init-misc)
;; Custom
(unless custom-file
  (load (setq custom-file (locate-user-emacs-file "custom.el")) t))

;; Show startup times
(add-hook 'window-setup-hook
          (lambda ()
            (message "window-setup: %.3fs, after-init: %.3fs"
                     (float-time (time-subtract nil before-init-time))
                     (float-time (time-subtract after-init-time before-init-time)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-vc-selected-packages
   '((org-gtd :url "https://github.com/Trevoke/org-gtd.el.git"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
