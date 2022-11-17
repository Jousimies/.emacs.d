;;; init-tray.el --- Awesome tray. -*- lexical-binding: t no-byte-compile: t -*-

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

;; Awesome-tray and echo-bar cannot show pdf page position, Modeline can.
;; So doom-modeline is better.

;;; Code:
(require 'awesome-tray)

(with-eval-after-load 'mu4e
  (add-to-list 'awesome-tray-active-modules "mail"))
(with-eval-after-load 'pdf-tools
  (add-to-list 'awesome-tray-active-modules "pdf-view-page"))

(setq awesome-tray-info-padding-right 2)
(add-hook 'after-init-hook 'awesome-tray-mode)

;; Echo-bar work like awesome-tray.
;; (when (maybe-require-package 'echo-bar)
;;   (setq echo-bar-format '(
;;                           "%e"
;;                           ;; mode-line-front-space
;;                           mode-line-mule-info
;;                           mode-line-client
;;                           mode-line-modified
;;                           mode-line-remote
;;                           mode-line-frame-identification
;;                           mode-line-buffer-identification "   "
;;                           mode-line-position
;;                           (vc-mode vc-mode)
;;                           ;; "  " mode-line-modes
;;                           mode-line-misc-info
;;                           mode-line-end-spaces)))

(provide 'init-tray)
;;; init-tray.el ends here
