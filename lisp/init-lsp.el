;;; init-lsp.el --- LSP -*- lexical-binding: t no-byte-compile: t -*-

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
(require 'lsp-bridge)

(setq lsp-bridge-mode-line nil)

(add-hook 'prog-mode-hook 'lsp-bridge-mode)
(add-hook 'org-mode-hook 'lsp-bridge-mode)
(add-hook 'lsp-bridge-mode-hook (lambda ()
                                  (company-mode -1)))

(provide 'init-lsp)
;;; init-lsp.el ends here
