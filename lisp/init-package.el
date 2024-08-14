;;; init-package.el --- Emacs package management     -*- lexical-binding: t; -*-

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

;; package.el & use-package
(setq package-archives '(("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
						 ("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
						 ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))
(setq package-archive-priorities '(("gnu" . 3)
                                   ("melpa" . 2)
                                   ("nongnu" . 1)))
(setq load-prefer-newer (not noninteractive))

(setq use-package-expand-minimally (not noninteractive))
(setq use-package-always-ensure t)
(setq use-package-always-defer t)

(package-initialize)

;; Auto Compile
(use-package auto-compile
  :hook ((after-init . auto-compile-on-load-mode)
         (after-init . auto-compile-on-save-mode)))


(provide 'init-package)
;;; init-package.el ends here
