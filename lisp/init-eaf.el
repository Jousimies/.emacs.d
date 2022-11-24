;;; init-eaf.el --- Emacs application framework -*- lexical-binding: t no-byte-compile: t -*-

;; Copyright (C) 2022 Duan Ning

;; Author: Duan Ning <duan_n@outlook.com>
;; Version: 1.0.0
;; Keywords:

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; https://emacs-china.org/t/emacs-linux-windows-mac-bsd/6199/2468
;; EAF 和 emacsclient 不能混着用。

;;

;;; Code:

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
(require 'eaf)
(setq eaf-config-location (concat user-emacs-directory "var/eaf"))
;; (setq eaf-proxy-type "socks5")
;; (setq eaf-proxy-host "127.0.0.1")
;; (setq eaf-proxy-port "1089")

(with-eval-after-load 'eaf
  (with-eval-after-load 'evil-collection
    (evil-collection-define-key 'normal 'eaf-pdf-outline-mode-map
      [return] 'eaf-pdf-outline-jump)
    (evil-collection-define-key 'normal 'eaf-pdf-outline-mode-map
      "q" 'quit-window)))

(setq eaf-mode-line-format '("%e"
                             (:eval
                              (doom-modeline-format--main))))

(require 'eaf-pdf-viewer)

;; Combine evil and eaf, auto change eaf-buffer to insert state.
;; https://emacs-china.org/t/eaf-evil/13089
(eval-after-load "evil"
  '(progn
     (defvar last-focus-buffer nil
       "Buffer currently in focus.")

     (defun buffer-focus-handler ()
       (interactive)
       (when (not (buffer-live-p last-focus-buffer))
         (setq last-focus-buffer nil))
       (when (and (eq (window-buffer (selected-window))
                      (current-buffer))
                  (not (eq last-focus-buffer (current-buffer))))
         (setq last-focus-buffer (current-buffer))
         (when (derived-mode-p 'eaf-mode)
           (evil-insert-state))))

     (add-hook 'buffer-list-update-hook #'buffer-focus-handler)))

;; eaf-browser can not login google account.
;; (require 'eaf-browser)

;; eaf-git make emacs startup very slow.
;; (require 'eaf-git)


(provide 'init-eaf)
;;; init-eaf.el ends here
