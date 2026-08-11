;; -*- lexical-binding: t; -*-
(add-hook 'window-setup-hook
          (lambda ()
            (message "window-setup: %.3fs, after-init: %.3fs"
                     (float-time (time-subtract nil before-init-time))
                     (float-time (time-subtract after-init-time before-init-time)))))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'package)
(setq package-archives
      '(("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")))
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Self define variables
(defconst cache-directory (expand-file-name ".cache" user-emacs-directory))
(defconst my-galaxy "C:/Users/Duan_/Documents/MyNotes/")

(require 'init-builtin)
(require 'init-edit)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
