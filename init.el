;;; init.el --- Emacs Initial Configuration          -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Duan Ning

;; Author: Duan Ning <duan_n@outlook.com>
;; Keywords: files

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

(add-to-list 'load-path "~/.emacs.d/lisp")
;; load-path
(add-to-list 'load-path "~/.emacs.d/packages/compat/")
(add-to-list 'load-path "~/.emacs.d/packages/dash.el/")
(add-to-list 'load-path "~/.emacs.d/packages/f.el/")
(add-to-list 'load-path "~/.emacs.d/packages/s.el/")
(add-to-list 'load-path "~/.emacs.d/packages/posframe/")
(add-to-list 'load-path "~/.emacs.d/packages/emacs-async/")

;; Define consts
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(defvar icloud "~/Library/Mobile Documents/"
  "This folder contains documents in icloud.")
(defvar nextcloud "~/Nextcloud"
  "This folder is My cloud.")
(defvar my-galaxy (expand-file-name "L.Personal.Galaxy" nextcloud)
  "This folder stores all the plain text files of my life.")
(defvar website-directory (expand-file-name "blogs_source/" my-galaxy)
  "The source folder of my blog.")
(defvar my-pictures (expand-file-name "pictures/" my-galaxy)
  "The folder save pictures.")
(defvar my-web_archive (expand-file-name "web_archive/" my-galaxy)
  "The folder save web pages.")
(defvar cache-directory (expand-file-name ".cache" user-emacs-directory))

(defvar my/proxy-ip "127.0.0.1")
(defvar my/proxy-port "1082")

(defvar my/tab-bar-right-string nil)

;; Require configuration
(require 'init-core)
(require 'init-builtin)

;; UI
(require 'init-tab)
(require 'init-modeline)
(require 'init-buffer)

;; Better Editor
(unless (display-graphic-p)
  (require 'init-evil))

(require 'init-edit)
(require 'init-completion)
(require 'init-search)
(require 'init-dired)

;; Programming
;; (require 'init-lsp)
(require 'init-prog)
(require 'init-shell)
(require 'init-git)

;; Note Everything based on org-mode
(require 'init-org)
(require 'init-note)
(require 'init-bib)
(require 'init-latex)
(require 'init-finance)
(require 'init-gtd)

;; Reading with Emacs
(require 'init-reader)
(require 'init-dict)

;; Applications
(require 'init-telega)
(require 'init-elfeed)
;; (require 'init-pass)

;; Plateform related configuration
(when IS-MAC
  (require 'init-mac))

;; Keybindings
(require 'init-keys)

;; Load custom.el, but It's empty.
(load (setq custom-file (locate-user-emacs-file "custom.el")) t)

(add-hook 'window-setup-hook
          (lambda ()
            (garbage-collect)
            (let ((curtime (current-time)))
              (message "Times: init:%.06f total:%.06f gc-done:%d"
                       (float-time (time-subtract after-init-time before-init-time))
                       (float-time (time-subtract curtime before-init-time))
                       gcs-done)))
          90)

(provide 'init)
;;; init.el ends here
