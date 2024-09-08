;;; init-media.el --- Media                          -*- lexical-binding: t; -*-

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
(use-package mpv
  :bind (("<f8>" . my/mpv-play-or-pause)
		 ("<f7>" . mpv-seek-backward)
		 ("<f9>" . mpv-seek-forward))
  :custom
  (mpv-default-options '("--http-proxy=http://127.0.0.1:7890"
						 "--ytdl-raw-options-append=proxy=http://127.0.0.1:7890"))
  :config
  (defun my/mpv-quit-with-save ()
	(interactive)
	(mpv-quit t))

  (defun my/mpv-toggle-progress ()
	(interactive)
	(mpv-run-command "keypress" "o"))

  (defun my/mpv-toggle-fullscreen ()
	(interactive)
	(mpv-run-command "keypress" "f"))

  (defun my/mpv-play-or-pause ()
	"Toggle between play and pause for mpv process."
	(interactive)
	(if (mpv-live-p)
		(mpv-pause)
      (if (eq major-mode 'dired-mode)
		  (mpv-play (dired-get-filename))
		(let ((file (read-file-name "File: ")))
		  (mpv-play file))))))

(use-package ready-player
  :hook (find-file . ready-player-mode))

(provide 'init-media)
;;; init-media.el ends here
