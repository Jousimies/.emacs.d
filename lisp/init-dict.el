;;; init-dict.el --- Dictionary -*- lexical-binding: t no-byte-compile: t -*-

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

;;; Code:

(when (maybe-require-package 'langtool)
  (setq langtool-http-server-host "localhost")
  (setq langtool-http-server-port 8081))

(when (maybe-require-package 'go-translate)
  (with-eval-after-load 'go-translate
    (setq gts-translate-list '(("en" "zh")))
    (setq gts-default-translator (gts-translator
                                  :picker (gts-noprompt-picker)
                                  :engines (list
                                            (gts-google-engine :parser (gts-google-summary-parser)))
                                  :render (gts-buffer-render))))
  (general-define-key
   :keymaps '(normal visual)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "ll" '(gts-do-translate :wk "Translate")))


(when (maybe-require-package 'lingva)
  (setq lingva-target "zh")

  (general-define-key
   :keymaps '(normal visual)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "lL" '(lingva-translate :wk "Lingva")))

;; (require 'dictionary-overlay)

(provide 'init-dict)
;;; init-dict.el ends here
