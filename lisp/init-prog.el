;;; init-prog.el --- Emacs Programming               -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Duan Ning

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

;; markdown
(use-package markdown-mode
  :load-path "packages/markdown-mode/"
  :mode (("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
			  ("C-c C-e" . markdown-do)))

;; swift
(use-package swift-mode
  :load-path "packages/swift-mode/"
  :mode ("\\.swift\\'" . swift-mode))

;; yaml
(use-package yaml-ts-mode
  :mode ("\\.yaml\\|\\.yml\\'" . yaml-ts-mode))

;; csv
(use-package csv-mode
  :load-path "packages/csv-mode/"
  :mode ("\\.csv\\'" . csv-mode))

(use-package rainbow-csv
  :load-path "packages/rainbow-csv/"
  :hook ((csv-mode . rainbow-csv-mode)
		 (tsv-mode . rainbow-csv-mode)))

;; (use-package demap
;;   :load-path "~/.emacs.d/packages/demap.el/"
;;   :hook (prog-mode . demap-toggle)
;;   :config
;;   (setq demap-minimap-window-side  'right)
;;   (setq demap-minimap-window-width 15))

(use-package visual-basic-mode
  :mode ("\\.vb\\|.bas\\'" . visual-basic-mode))

(provide 'init-prog)
;;; init-prog.el ends here
