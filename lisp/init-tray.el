;;; init-tray.el --- tray instead of modeline        -*- lexical-binding: t; -*-

;; Copyright (C) 2025  DN

;; Author: DN <duan_n@outlook.com>
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

(use-package nano-modeline
  :load-path "packages/nano-modeline/"
  :hook ((prog-mode . nano-modeline-prog-mode)
	 (org-mode . nano-modeline-org-mode))
  :custom
  (nano-modeline-position 'nano-modeline-footer))

;; (add-hook 'after-init-hook #'window-divider-mode)
;; (add-to-list `load-path (expand-file-name "packages/awesome-tray" user-emacs-directory))
;; (unless (featurep 'awesome-tray-mode)
;;   (require 'awesome-tray))
;; (add-hook 'window-divider-mode-hook #'one/awesome-tray-minimal-enable)
;; (setq awesome-tray-active-modules '("belong" "file-path" "mode-name" "battery" "date"))

;; (defun one/awesome-tray-minimal-enable ()
;;   (interactive)
;;   (dolist (buf '(" *Echo Area 0*" " *Echo Area 1*"))
;;     (with-current-buffer (get-buffer-create buf)
;;       (remove-overlays (point-min) (point-max))
;;       (push (make-overlay (point-min) (point-max) nil nil t)
;;             awesome-tray-overlays)))

;;   (when awesome-tray-update-interval
;;     (run-with-timer 0 awesome-tray-update-interval 'awesome-tray-update)))

(provide 'init-tray)
;;; init-tray.el ends here
