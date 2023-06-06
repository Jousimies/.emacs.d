;; init-defaults.el --- Defaults. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

;; Variables defined in C source code
(setq ring-bell-function 'ignore)
(setq tab-width 4)
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq use-short-answers t)
(setq read-process-output-max #x10000)
(setq create-lockfiles nil)
(setq recenter-redisplay nil)
(setq load-prefer-newer t)
(setq next-screen-context-lines 5)
(setq frame-inhibit-implied-resize t)
(setq inhibit-compacting-font-caches t)
(setq inhibit-quit nil)
(setq fast-but-imprecise-scrolling t)
(setq scroll-preserve-screen-position 'always)
(setq auto-save-list-file-name nil)
(setq history-delete-duplicates t)
(setq bidi-display-reordering nil)
(setq read-buffer-completion-ignore-case t)
(setq completion-ignore-case t)
(setq delete-by-moving-to-trash t)
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(setq max-mini-window-height 10)

;; Define some variables to facilitate the location of configuration files or related settings for specific systems.
(defvar my-cloud "~/Nextcloud"
  "This folder is My cloud.")

;; L.Personal.Galaxy location may change, but folders in this directory never change.
(defvar my-galaxy (expand-file-name "L.Personal.Galaxy" my-cloud)
  "This folder stores all the plain text files of my life.")

(defvar website-directory (expand-file-name "blogs_source/" my-galaxy)
  "The source folder of my blog.")

(defvar my/web_archive (expand-file-name "web_archive/" my-galaxy)
  "The folder save web pages.")

(defvar my/reference-lists `(,(concat my-galaxy "/bibtexs/References.bib")
                             ,(concat my-cloud "/L.Calibre/calibre.bib")))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(use-package server
  :hook (after-init . server-start))

(use-package socks
  :init
  (setq socks-override-functions 1)
  (setq socks-noproxy '("localhost"))
  :config
  (setq url-gateway-method 'socks)
  (setq socks-server '("Default server" "127.0.0.1" 1080 5)))

(defun toggle-proxy ()
  "Toggle proxy for the url.el library."
  (interactive)
  (if url-proxy-services
      (proxy-disable)
    (proxy-enable)))

(defun proxy-enable ()
  "Enable proxy."
  (interactive)
  (setq url-proxy-services
        '(("http" . "127.0.0.1:8118")
          ("https" . "127.0.0.1:8118")
          ("socks" . "127.0.0.1:8118")
          ("no_proxy" . "0.0.0.0")))
  (message "Proxy enabled! %s" (car url-proxy-services)))

(defun proxy-disable ()
  "Disable proxy."
  (interactive)
  (if url-proxy-services
      (setq url-proxy-services nil))
  (message "Proxy disabled!"))

(run-with-idle-timer 2 nil (lambda ()
                             (proxy-enable)))

(use-package hydra
  :commands defhydra
  :config
  (setq hydra-hint-display-type 'lv)
  (setq hydra-posframe-show-params `(:poshandler posframe-poshandler-frame-center
                                                 :internal-border-width 2
                                                 :internal-border-color "#61AFEF"
                                                 :left-fringe 16
                                                 :right-fringe 16)))

(use-package psearch
  :commands psearch-replace psearch-patch)

(use-package epkg
  :commands epkg-describe-package
  :init
  (add-to-list 'display-buffer-alist '("^\\*Help\\*"
                                       (display-buffer-in-tab)
                                       (side . right)
                                       (window-width . 0.5)
                                       (window-parameters
                                        (select . t)
                                        (mode-line-format . none))))
  :config
  (setq epkg-repository (expand-file-name "cache/epkgs" user-emacs-directory)))

(use-package epkg-marginalia
  :after (epkg marginalia)
  :config
  (cl-pushnew 'epkg-marginalia-annotate-package
              (alist-get 'package marginalia-annotator-registry)))

(use-package request
  :config
  (setq request-storage-directory (expand-file-name "cache/request" user-emacs-directory)))

(provide 'init-defaults)
;;; init-defaults.el ends here.
