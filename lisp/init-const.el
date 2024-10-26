;;; init-const.el --- Const variables                -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Duan Ning

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
;; plateform
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

;; Define some variables to facilitate the location of configuration files or related settings for specific systems.
(when IS-MAC
  (defvar icloud "~/Library/Mobile Documents/"
    "This folder contains documents in icloud.")
  (defvar nextcloud "~/Nextcloud"
    "This folder is My cloud.")
  ;; L.Personal.Galaxy location may change, but folders in this directory never change.
  (defvar my-galaxy (expand-file-name "L.Personal.Galaxy" nextcloud)
    "This folder stores all the plain text files of my life.")
  (defvar website-directory (expand-file-name "blogs_source/" my-galaxy)
    "The source folder of my blog.")
  (defvar my-pictures (expand-file-name "pictures/" my-galaxy)
    "The folder save pictures.")
  (defvar my-web_archive (expand-file-name "web_archive/" my-galaxy)
    "The folder save web pages.")
  (defvar cache-directory (expand-file-name ".cache" user-emacs-directory)))

(when IS-WINDOWS
  (defvar icloud "U:/Cloud/"
    "This folder contains documents in icloud.")
  (defvar nextcloud "U:/Cloud/"
    "This folder is My cloud.")
  ;; L.Personal.Galaxy location may change, but folders in this directory never change.
  (defvar my-galaxy (expand-file-name nextcloud)
    "This folder stores all the plain text files of my life.")
  (defvar website-directory (expand-file-name "blogs_source/" my-galaxy)
    "The source folder of my blog.")
  (defvar my-pictures (expand-file-name "pictures/" my-galaxy)
    "The folder save pictures.")
  (defvar my-web_archive (expand-file-name "web_archive/" my-galaxy)
    "The folder save web pages.")
  (defvar cache-directory (expand-file-name ".cache" user-emacs-directory)))

;; Proxy
(defvar my/proxy-ip "127.0.0.1")
(defvar my/proxy-port "7890")

;; Tab-bar
(defvar my/tab-bar-right-string nil)

(provide 'init-const)
;;; init-const.el ends here
