;;; init-mac.el --- Mac related configuration        -*- lexical-binding: t; -*-

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
(when (memq window-system '(mac ns))

  (setopt trash-directory "~/.Trash"
		  ns-use-proxy-icon nil
		  ns-pop-up-frames nil
		  ns-use-srgb-colorspace nil)

  (use-package color-picker
	:commands color-picker))


(provide 'init-mac)
;;; init-mac.el ends here
