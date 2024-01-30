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

(add-hook 'after-init-hook #'tab-bar-mode)
(global-set-key (kbd "C-c b t") #'tab-switch)
(global-set-key (kbd "s-t") #'tab-new)
(global-set-key (kbd "s-w") #'tab-close)

(setopt tab-bar-new-tab-choice 'scratch-buffer
		tab-bar-close-button-show nil
		;; tab-bar-separator "​​"
		tab-bar-select-tab-modifiers '(super)
		tab-bar-tab-hints t)

(defvar my/tab-bar-right-string)
(defun my/tab-bar-time-update (&rest rest)
  (format-time-string "%a %b %d %H:%M "))

(setq my/tab-bar-right-string '((:eval (my/tab-bar-time-update))))

(defun my/tab-bar-format-right ()
  `((global menu-item ,(format-mode-line my/tab-bar-right-string) ignore)))

(add-hook 'after-init-hook 'display-battery-mode)
(add-to-list 'my/tab-bar-right-string 'battery-mode-line-string t)

(setopt tab-bar-format '(tab-bar-format-tabs
						 tab-bar-separator
						 tab-bar-format-align-right
						 my/tab-bar-format-right))

(defun tab-bar-format-menu-bar ()
  "Produce the Menu button for the tab bar that shows the menu bar."
  `((menu-bar menu-item (propertize "  " 'face nil)
              tab-bar-menu-bar :help "Menu Bar")))

(defun my/tab-bar-tab-name-format-comfortable (tab i)
  (propertize (concat " " (tab-bar-tab-name-format-default tab i) " ")
              'face (funcall tab-bar-tab-face-function tab)))

(setq tab-bar-tab-name-format-function #'my/tab-bar-tab-name-format-comfortable)

(provide 'init-tab)
;;; init-tab.el ends here
