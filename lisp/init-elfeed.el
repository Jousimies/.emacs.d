;;; init-elfeed.el --- RSS Reader                    -*- lexical-binding: t; -*-

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
(defun z/xwidget-webkit-browse-entry-link-at-point ()
  (interactive)
  (let ((entry-link
         (if (eq major-mode 'elfeed-search-mode)
             (elfeed-entry-link (elfeed-search-selected t))
           (elfeed-entry-link elfeed-show-entry))))
    (xwidget-webkit-browse-url entry-link)))

(defun elfeed-display-buffer (buf &optional act)
  (pop-to-buffer buf '((display-buffer-reuse-window display-buffer-in-side-window)
                       (side . bottom)
                       (window-height . 0.8)
                       (reusable-frames . visible)
                       (window-parameters
                        (select . t)
                        (quit . t)
                        (popup . t)))))

(defun nerd-icon-for-tags (tags)
  "Generate Nerd Font icon based on tags.
  Returns default if no match."
  (cond ((member "youtube" tags)  (nerd-icons-faicon "nf-fa-youtube_play" :face '(:foreground "#FF0200")))
        ((member "instagram" tags) (nerd-icons-faicon "nf-fa-instagram" :face '(:foreground "#FF00B9")))
        ((member "emacs" tags) (nerd-icons-sucicon "nf-custom-emacs" :face '(:foreground "#9A5BBE")))
        ((member "github" tags) (nerd-icons-faicon "nf-fa-github"))
        (t (nerd-icons-faicon "nf-fae-feedly" :face '(:foreground "#2AB24C")))))

(defun lucius/elfeed-search-print-entry--better-default (entry)
  "Print ENTRY to the buffer."
  (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
         (date-width (car (cdr elfeed-search-date-format)))
         (title (concat (or (elfeed-meta entry :title)
                            (elfeed-entry-title entry) "")
                        ;; NOTE: insert " " for overlay to swallow
                        " "))
         (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
         (feed (elfeed-entry-feed entry))
         (feed-title (when feed (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
         (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
         (tags-str (mapconcat (lambda (s) (propertize s 'face 'elfeed-search-tag-face)) tags ","))
         (title-width (- (frame-width)
                         ;; (window-width (get-buffer-window (elfeed-search-buffer) t))
                         date-width elfeed-search-trailing-width))
         (title-column (elfeed-format-column
                        title (elfeed-clamp
                               elfeed-search-title-min-width
                               title-width
                               elfeed-search-title-max-width) :left))


         ;; Title/Feed ALIGNMENT
         (align-to-feed-pixel (+ date-width
                                 (max elfeed-search-title-min-width
                                      (min title-width elfeed-search-title-max-width)))))
    (insert (propertize date 'face 'elfeed-search-date-face) " ")
    (insert (propertize title-column 'face title-faces 'kbd-help title))
    (put-text-property (1- (point)) (point) 'display `(space :align-to ,align-to-feed-pixel))
    ;; (when feed-title (insert " " (propertize feed-title 'face 'elfeed-search-feed-face) " "))
    (when feed-title
      (insert " " (concat (nerd-icon-for-tags tags) " ")
              (propertize feed-title 'face 'elfeed-search-feed-face) " "))
    (when tags (insert "(" tags-str ")"))))

;; elfeed
(use-package elfeed
  :load-path "packages/elfeed/"
  :commands elfeed
  :bind ((:map elfeed-search-mode-map
			   ("U" . elfeed-update)))
  :config
  (setq elfeed-use-curl nil)
  (setq elfeed-show-entry-switch #'elfeed-display-buffer)
  (setq elfeed-search-print-entry-function #'lucius/elfeed-search-print-entry--better-default)
  (use-package elfeed-org
    :load-path "packages/elfeed-org/"
    :config
	(elfeed-org)
    (setq rmh-elfeed-org-files `(,(concat my-galaxy "/denote/20230330T120149==5d2b3--rss-sources__elfeed_emacs.org")))))

(use-package elfeed-tube
  :load-path "packages/elfeed-tube/" "packages/emacs-aio/"
  :after elfeed
  :bind ((:map elfeed-show-mode-map
			   ("C-c C-f" . elfeed-tube-mpv-follow-mode)
			   ("C-c C-w" . elfeed-tube-mpv-where)
			   ("F" . elfeed-tube-fetch)
			   ("m" . elfeed-tube-mpv)
			   ([remap save-buffer] . elfeed-tube-save))
		 (:map elfeed-search-mode-map
			   ("F" . elfeed-tube-fetch)
			   ([remap save-buffer] . elfeed-tube-save)))
  :config
  (elfeed-tube-setup))

(use-package elfeed-tube-mpv
  :commands elfeed-tube-mpv-follow-mode elfeed-tube-mpv-where elfeed-tube-mpv)

(use-package mpv
  :commands mpv-start
  :load-path "packages/mpv.el/"
  :bind (("<f8>" . my/mpv-play-or-pause)
		 ("C-<f8>" . mpv-toggle-video)
		 ("M-<f8>" . my/mpv-toggle-progress)
		 ("s-<f8>" . my/mpv-quit-with-save)
		 ("<f7>" . mpv-seek-backward)
		 ("<f9>" . mpv-seek-forward)
		 ("C-<f7>" . mpv-speed-decrease)
		 ("C-<f9>" . mpv-speed-increase))
  :config
  (defun my/mpv-quit-with-save ()
	(interactive)
	(mpv-quit t))

  (defun my/mpv-toggle-progress ()
	(interactive)
	(mpv-run-command "keypress" "o"))

  (defun my/mpv-play-or-pause ()
	"Toggle between play and pause for mpv process."
	(interactive)
	(if (mpv-live-p)
		(mpv-pause)
      (if (eq major-mode 'dired-mode)
		  (mpv-play (dired-get-filename))
		(let ((file (read-file-name "File: ")))
		  (mpv-play file)))))
  (setq mpv-default-options '("--http-proxy=http://127.0.0.1:7890"
							  "--ytdl-raw-options-append=proxy=http://127.0.0.1:7890")))

;;;###autoload
(defun my/dired-open-with-mpv ()
  (interactive)
  (mpv-start (dired-get-filename)))

;; @Lucius_Chen
(defun elfeed/open-link-with-mpv ()
  "Open the link at point with mpv if it is a video."
  (interactive)
  (let ((url (or (elfeed-get-link-at-point)
                 (elfeed-get-url-at-point))))
    (if (and url (string-match "\\(?:\\.\\(mp4\\|webm\\|ogg\\|avi\\|mkv\\)\\)?" url))
        (progn
          (message "%s" (propertize "Starting mpv, please wait!" 'face 'elfeed-log-info-level-face))
          (mpv-play-url url))
      (message "%s" (propertize "Not a video link!" 'face 'elfeed-log-warn-level-face)))))

(with-eval-after-load 'elfeed
  (define-key elfeed-show-mode-map (kbd "o") #'elfeed/open-link-with-mpv))

(provide 'init-elfeed)
;;; init-elfeed.el ends here
