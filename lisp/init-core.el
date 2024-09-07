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
(setopt ;; initial-major-mode 'fundamental-mode
        inhibit-startup-screen t
        ;; (setq ring-bell-function 'ignore)
        ring-bell-function (lambda ()
                             (invert-face 'mode-line)
                             (run-with-timer 0.05 nil 'invert-face 'mode-line))
        use-file-dialog nil
        use-dialog-box nil
        use-short-answers t
        read-process-output-max #x10000
        create-lockfiles nil
        recenter-redisplay nil
        next-screen-context-lines 5
        inhibit-compacting-font-caches t
        frame-resize-pixelwise t
        inhibit-quit nil
        fast-but-imprecise-scrolling t
        scroll-preserve-screen-position 'always
        auto-save-list-file-name nil
        history-length 1000
        history-delete-duplicates t
        bidi-display-reordering nil
        read-buffer-completion-ignore-case t
        completion-ignore-case t
        delete-by-moving-to-trash t
        minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)
        redisplay-skip-fontification-on-input t
        cursor-in-non-selected-windows nil)

(setq-default initial-scratch-message nil)

;; Language Environment
(set-language-environment "UTF-8")
(setq default-input-method nil)

;; System Coding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Font setting.
;; `set-face-attribute' 设置默认字体
;; 对于中英文字体无法做到等宽和等高，两者只能取其一。相对而言，等宽更重要一些。
;; 不等高会导致 modeline 跳动，可以在 modeline 中插入中文字体“丨”[gun]
;; 字体搭配1: Cascadia Next SC
;; 字体搭配2: Latin Modern Mono 和 Source Han Serif SC
(set-face-attribute 'default nil :family "Cascadia Next SC" :height 160)

;; Unicode
;; `set-fontset-font' 用于指定某些字符集使用特定的字体
(set-fontset-font t 'unicode (font-spec :family "Symbols Nerd Font Mono" :size 14) nil 'prepend)

;; 设置中文字集
;; `han': 汉字字符集，主要用于简体中文和繁体中文字符
;; `cjk-misc': CJK（中日韩）字符集中的其他字符，包含了少量的中文、日文、韩文字符
;; `kana': 日文假名字符集，但在处理与中文相关的文档时可能偶尔用到
;; `bopomofo': 注音符号字符集，用于台湾地区的汉字注音
(dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family "Cascadia Next SC")))

;; Emoji
;; According to https://github.com/domtronn/all-the-icons.el
;; Use 'prepend for the NS and Mac ports or Emacs will crash.
(set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji" :size 14) nil 'prepend)
(set-fontset-font t 'symbol (font-spec :family "Symbols" :size 14) nil 'prepend)

;; 除以上方法，也可以使用 `variable-pitch-mode'
;; (set-face-attribute 'variable-pitch nil :family "TsangerJinKai02" :height 160)
;; (set-face-attribute 'fixed-pitch nil :family "SF Mono" :height 160)
;; (add-hook 'text-mode-hook #'variable-pitch-mode)

;; Proxy
(use-package socks
  :ensure nil
  :defer 2
  :custom
  (url-gateway-method 'socks)
  (socks-noproxy '("localhost"))
  (socks-server `("Default server" ,my/proxy-ip ,(string-to-number my/proxy-port) 5))
  (url-proxy-services `(("http" . ,(concat my/proxy-ip ":" my/proxy-port))
                        ("https" . ,(concat my/proxy-ip ":" my/proxy-port))
                        ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  :config
  (setenv "all_proxy" (concat "socks5://" my/proxy-ip ":" my/proxy-port)))

(use-package server
  :ensure nil
  :config
  (unless (server-running-p)
    (server-start)))

(use-package async)

;; Better emacs garbage collect behavior
(use-package gcmh
  :hook (on-first-buffer . gcmh-mode)
  :custom
  (gc-cons-percentage 0.1)
  (gcmh-verbose nil)
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold #x1000000))

(advice-add 'after-focus-change-function :after 'garbage-collect)


(provide 'init-core)
;;; init-core.el ends here
