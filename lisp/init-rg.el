;;; init-rg.el --- rg -*- lexical-binding: t no-byte-compile: t -*-

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

;;
(when (maybe-require-package 'rg)
  (add-hook 'on-first-input-hook 'rg-enable-default-bindings)
  (with-eval-after-load 'rg
    (setq rg-group-result t)
    (setq rg-show-columns t))
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "sr" '(rg :wk "rg")))


;; (require 'blink-search)
;;; Code:

(provide 'init-rg)
;;; init-rg.el ends here
