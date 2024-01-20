;;; init-pass.el --- Password management             -*- lexical-binding: t; -*-

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

;; pass mode 存在一些问题， edit 时会卡死 Emacs 。

(use-package password-store
  :load-path "packages/password-store/contrib/emacs/"
  :commands (password-store-copy
			 password-store-edit
			 password-store-url
			 password-store-clear
			 password-store-insert
			 password-store-remove
			 password-store-rename
			 password-store-generate
			 password-store-copy-field
			 password-store-generate-no-symbols))


(provide 'init-pass)
;;; init-pass.el ends here
