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


(setopt trash-directory "~/.Trash"
		ns-use-proxy-icon nil
		ns-pop-up-frames nil
		ns-use-srgb-colorspace nil)

(use-package color-picker
  :commands color-picker)

;; (use-package mac-plugin
;; 	:load-path "packages/EmacsMacPluginModule/"
;; 	:config
;; 	(setq macos-project-root "~/.emacs.d/packages/EmacsMacPluginModule/")
;; 	(mac-plugin-load-release)
;; 	(atmosphere-enable)
;; 	(mac-plugin-set-cursor-color "#fcc800")
;; 	(mac-plugin-set-shadow-opacity 1.0))

(use-package macos
  :load-path "packages/EmacsMacOSModule/"
  :commands macos-reveal-in-finder macos-share
  :config
  (setq macos-module-install-dir (expand-file-name "modules" user-emacs-directory)
		macos-module-path (expand-file-name "libEmacsMacOSModule.dylib" macos-module-install-dir))
  (load-file macos-module-path))

(use-package emt
  :load-path "packages/emt"
  :bind (("M-f" . emt-forward-word)
         ("M-b" . emt-backward-word)
         ("M-d" . emt-kill-word)
         ("M-h" . emt-backward-kill-word))
  :hook (on-first-input . emt-ensure))

;; use patch instead of macism.
;; https://github.com/LuciusChen/.emacs.d/blob/main/patches/ns-mac-input-source.patch
(defvar current-system-input-method nil)

(defun ime-select-rime ()
  (interactive)
  (mac-select-input-source "im.rime.inputmethod.Squirrel.Hans")
  (setq cursor-type 'bar)
  (set-cursor-color "red")
  (setq current-system-input-method t))

(defun ime-select-abc ()
  (interactive)
  (mac-select-input-source "com.apple.keylayout.ABC")
  (setq cursor-type 'box)
  (set-cursor-color (foreground-color-at-point))
  (setq current-system-input-method nil))

(defun ime-switch ()
  (interactive)
  (if current-system-input-method
      (progn
		(ime-select-abc)
		(force-mode-line-update))
	(progn
      (ime-select-rime)
      (force-mode-line-update))))

(add-hook 'on-switch-buffer-hook 'ime-select-abc)

(global-set-key (kbd "C-\\") 'ime-switch)

(provide 'init-mac)
;;; init-mac.el ends here
