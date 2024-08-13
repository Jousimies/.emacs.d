;;; init-eaf.el --- Emacs application                -*- lexical-binding: t; -*-

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
(use-package holo-layer
  :load-path "~/.emacs.d/packages/holo-layer/"
  :commands holo-layer-enable
  :hook (on-first-buffer . holo-layer-enable)
  :custom
  (holo-layer-python-command "~/.env/bin/python3")
  (holo-layer-enable-cursor-animation nil)
  (holo-layer-enable-place-info t)
  (holo-layer-enable-type-animation t))

;; (use-package eaf
;;   :load-path "~/.emacs.d/packages/emacs-application-framework/"
;;   :custom
;;   (eaf-python-command "~/.env/bin/python3")
;;   :config
;;   (require 'eaf-pdf-viewer)
;;   (require 'eaf-browser)
;;   (require 'eaf-file-manager))


(provide 'init-eaf)
;;; init-eaf.el ends here
