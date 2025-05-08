;;; init-tab.el --- Tab bar                          -*- lexical-binding: t; -*-

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
(use-package tab-bar
  :hook (emacs-startup . tab-bar-mode)
  :custom
  (tab-bar-auto-width nil)
  ;; (tab-bar-auto-width-max '((160) 20))
  (tab-bar-new-tab-choice 'scratch-buffer)
  (tab-bar-close-button-show t)
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-separator " ")
  (tab-bar-select-tab-modifiers '(super))
  (tab-bar-tab-hints t))

(setq my/tab-bar-right-string '((:eval global-mode-string)))

(defun my/tab-bar-format-right ()
  `((global menu-item ,(format-mode-line my/tab-bar-right-string) ignore)))

(setopt tab-bar-format '(tab-bar-format-tabs
			 tab-bar-separator
			 tab-bar-format-align-right
			 my/tab-bar-format-right))

(defun tab-bar-format-menu-bar ()
  "Produce the Menu button for the tab bar that shows the menu bar."
  `((menu-bar menu-item (propertize "  " 'face nil)
              tab-bar-menu-bar :help "Menu Bar")))

;; (use-package tab-line
;;   :hook (on-first-buffer . global-tab-line-mode))


(provide 'init-tab)
;;; init-tab.el ends here
