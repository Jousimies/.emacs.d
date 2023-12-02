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
  (set-fontset-font t 'unicode (font-spec :family "Apple Color Emoj" :size 11.5) nil 'prepend)

  (set-fontset-font t 'unicode (font-spec :family "Hack Nerd Font Mono" :size 14) nil 'prepend)

  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset (font-spec :family "TsangerJinKai02" :height 140))
    t 'prepend))

;; start server, so can use emaclient to edit file outside emacs
(require 'server)
(add-hook 'after-init-hook (lambda ()
                             (unless (server-running-p)
                               (server-start))))

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
        max-mini-window-height 10
        redisplay-skip-fontification-on-input t
        echo-keystrokes 0.25
        cursor-in-non-selected-windows nil)

;; Proxy
(with-eval-after-load 'url-vars
  (setq url-proxy-services
        '(("http" . "127.0.0.1:7890")
          ("https" . "127.0.0.1:7890")
          ("socks" . "127.0.0.1:7890")
          ("no_proxy" . "0.0.0.0"))))

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
