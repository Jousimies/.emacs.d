;; -*- lexical-binding: t; -*-
(add-hook 'window-setup-hook
          (lambda ()
            (message "window-setup: %.3fs, after-init: %.3fs"
                     (float-time (time-subtract nil before-init-time))
                     (float-time (time-subtract after-init-time before-init-time)))))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;; (unless package-archive-contents
;;   (package-refresh-contents))

;; (setq package-check-signature nil)

;; Benchmark
;; (use-package benchmark-init
;;   :ensure t
;;   :config
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package setup
  :ensure t)

;; Self define variables
(defconst cache-directory (expand-file-name ".cache" user-emacs-directory))
(defconst my-galaxy "C:/Users/Duan_/Documents/MyNotes/")

(require 'init-setup)

(require 'init-builtin)
(require 'init-edit)
(require 'init-ui)
(require 'init-completion)

(unless custom-file
  (load (setq custom-file (locate-user-emacs-file "custom.el")) t))


