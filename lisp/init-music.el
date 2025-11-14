;;; init-music.el --- Music                          -*- lexical-binding: t; -*-

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

(add-to-list 'load-path "~/.emacs.d/packages/emacs-request/")
(add-to-list 'load-path "~/.emacs.d/packages/anaphora/")
(add-to-list 'load-path "~/.emacs.d/packages/emacs-memoize/")
(add-to-list 'load-path "~/.emacs.d/packages/elquery/")
(add-to-list 'load-path "~/.emacs.d/packages/versuri/")
(add-to-list 'load-path "~/.emacs.d/packages/Emacs-esqlite/")
(add-to-list 'load-path "~/.emacs.d/packages/Emacs-pcsv/")
(add-to-list 'load-path "~/.emacs.d/packages/lastfm.el/")

(with-eval-after-load 'request
  (setopt request-storage-directory (expand-file-name "request" cache-directory)))

(use-package vuiet
  :load-path "~/.emacs.d/packages/vuiet/"
  :commands vuiet-play-artist)


(provide 'init-music)
;;; init-music.el ends here
