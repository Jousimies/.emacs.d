;;; init-core.el --- Emacs core configuration        -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Duan Ning

;; Author: Duan Ning <duan_n@outlook.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; load-path
(add-to-list 'load-path "~/.emacs.d/packages/compat/")
(add-to-list 'load-path "~/.emacs.d/packages/dash.el/")
(add-to-list 'load-path "~/.emacs.d/packages/f.el/")
(add-to-list 'load-path "~/.emacs.d/packages/s.el/")
(add-to-list 'load-path "~/.emacs.d/packages/posframe/")
(add-to-list 'load-path "~/.emacs.d/packages/emacs-async/")

;; show startup time at `scratch' buffer.
(defun my/packages-installed (load-path)
  (let ((my/packages 0))
    (dolist (path load-path)
      (when (not (string-prefix-p "/Applications/" path))
        (setq my/packages (1+ my/packages))))
    my/packages))

(add-hook 'window-setup-hook
          (lambda ()
            (garbage-collect)
            (let ((curtime (current-time)))
              (with-current-buffer "*scratch*"
                (goto-char (point-max))
                (insert
                 (concat "\n"
                         (format ";; Emacs Startup Times: init:%.03f total:%.03f gc-done:%d"
                                 (float-time (time-subtract after-init-time before-init-time))
                                 (float-time (time-subtract curtime before-init-time))
                                 gcs-done)
                         "\n"
                         (format ";; Total Packages Required: %d" (my/packages-installed load-path))
                         "\n\n"))
                90))))

;; on.el
(add-to-list 'load-path "~/.emacs.d/packages/on.el/")
(require 'on)

;; system coding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Fonts
(when (display-graphic-p)
  (set-face-attribute 'default nil :font "Iosevka Term" :weight 'normal :height 140)

  ;; Source Han Serif SC -> TsangerJinKai02
  (set-fontset-font t 'unicode (font-spec :family "Apple Color Emoj" :size 12) nil 'prepend)

  (set-fontset-font t 'unicode (font-spec :family "Hack Nerd Font Mono" :size 14) nil 'prepend)

  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset (font-spec :family "Source Han Serif SC" :height 140))
    t 'prepend))

;; icons
(use-package nerd-icons
  :load-path "packages/nerd-icons.el/"
  :commands nerd-icons-codicon nerd-icons-faicon nerd-icons-icon-for-file
  :config
  (setq nerd-icons-font-family "Hack Nerd Font Mono"))

(use-package nerd-icons-completion
  :load-path "packages/nerd-icons-completion/"
  :hook (minibuffer-setup . nerd-icons-completion-mode))

(use-package nerd-icons-dired
  :load-path "packages/emacs-nerd-icons-dired"
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :load-path "packages/nerd-icons-ibuffer/"
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;; Define some variables to facilitate the location of configuration files or related settings for specific systems.
(defvar mobile-document "~/Library/Mobile Documents/"
  "This folder contains documents in icloud.")

(defvar my-cloud "~/Nextcloud"
  "This folder is My cloud.")

;; L.Personal.Galaxy location may change, but folders in this directory never change.
(defvar my-galaxy (expand-file-name "L.Personal.Galaxy" my-cloud)
  "This folder stores all the plain text files of my life.")

(defvar website-directory (expand-file-name "blogs_source/" my-galaxy)
  "The source folder of my blog.")

(defvar my/web_archive (expand-file-name "web_archive/" my-galaxy)
  "The folder save web pages.")

(defvar cache-directory (expand-file-name ".cache" user-emacs-directory))

;; (setq inhibit-default-init t)
;; (setq inhibit-splash-screen t)
(setopt initial-major-mode 'fundamental-mode
        inhibit-startup-screen t
        ;; (setq ring-bell-function 'ignore)
        ring-bell-function (lambda ()
                             (invert-face 'mode-line)
                             (run-with-timer 0.05 nil 'invert-face 'mode-line))
        tab-width 4
        use-file-dialog nil
        use-dialog-box nil
        use-short-answers t
        read-process-output-max #x10000
        create-lockfiles nil
        recenter-redisplay nil
        load-prefer-newer t
        next-screen-context-lines 5
        frame-inhibit-implied-resize t
        inhibit-compacting-font-caches t
        frame-resize-pixelwise t
        inhibit-quit nil
        fast-but-imprecise-scrolling t
        scroll-preserve-screen-position 'always
        auto-save-list-file-name nil
        history-delete-duplicates t
        bidi-display-reordering nil
        read-buffer-completion-ignore-case t
        completion-ignore-case t
        delete-by-moving-to-trash t
        minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)
        ;; max-mini-window-height 10
        redisplay-skip-fontification-on-input t
        cursor-in-non-selected-windows nil)

(setq-default initial-scratch-message
              (propertize
               (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you") 'face 'italic))

;; Proxy
(add-to-list 'process-environment "https_proxy=http://localhost:7890")
(add-to-list 'process-environment "http_proxy=http://localhost:7890")
(add-to-list 'process-environment "no_proxy=localhost,127.0.0.0,127.0.0.1,127.0.1.1,local.home")

;; start server, so can use emaclient to edit file outside emacs
(require 'server)
(add-hook 'after-init-hook (lambda ()
                             (unless (server-running-p)
                               (server-start))))

;; Better emacs garbage collect behavior
(use-package gcmh
  :load-path "packages/gcmh"
  :hook ((after-init . gcmh-mode)
         (focus-out . garbage-collect))
  :config
  (setq gc-cons-percentage 0.1)
  (setq gcmh-idle-delay 'auto)
  (setq gcmh-auto-idle-delay-factor 10)
  (setq gcmh-high-cons-threshold #x1000000))


(provide 'init-core)
;;; init-core.el ends here
