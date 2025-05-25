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
  :hook (after-init . emt-ensure))

(use-package macim
  :load-path "packages/macim.el/"
  :hook ((after-init . macim-mode)
	 (minibuffer-mode . macim-select-ascii)
	 (isearch-mode . macim-select-ascii))
  :custom
  (macim-other "im.rime.inputmethod.Squirrel.Hans")
  :config
  (defvar my/macim-context-ignore-modes '("telega-root-mode"
					  "telega-image-mode"
					  "mu4e-headers-mode"
					  "mu4e-view-mode"
					  "elfeed-show-mode"
					  "elfeed-search-mode"))
  (defun +macim-context-ignore-modes ()
    (let ((mode (symbol-name major-mode)))
      (when (member mode my/macim-context-ignore-modes))
      'ascii))

  (add-to-list 'macim-context-early-predicates #'+macim-context-ignore-modes))


(provide 'init-mac)
;;; init-mac.el ends here
