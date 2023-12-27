;;; init-dired.el --- Dired                          -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Duan Ning

;; Author: Duan Ning <jousimies@DNs-Air.local>
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

(when (and (eq system-type 'darwin) (executable-find "gls"))
    (setopt dired-use-ls-dired nil
			insert-directory-program "gls"
			dired-listing-switches
			"-l --almost-all --human-readable --group-directories-first --no-group"))

(setopt dired-dwim-target t
		dired-auto-revert-buffer #'dired-buffer-stale-p
		dired-recursive-copies 'always
		dired-recursive-deletes 'top
		dired-auto-revert-buffer t)

;; dired-do-shell-command, open file with default application.
(let ((cmd (cond ((and (eq system-type 'darwin) (display-graphic-p)) "open")
                   ((and (eq system-type 'gnu/linux) (display-graphic-p)) "xdg-open")
                   ((and (eq system-type 'windows-nt) (display-graphic-p)) "start")
                   (t ""))))
    (setq dired-guess-shell-alist-user
          `(("\\.\\(?:docx\\|doc\\|xlsx\\|xls\\|ppt\\|pptx\\)\\'" ,cmd)
			("\\.\\(?:eps\\|dwg\\|psd\\|drawio)'" ,cmd)
            ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd))))

(add-hook 'dired-mode-hook
          (lambda () (setq-local truncate-lines t)))

(defun z/dired-insert-date-folder ()
  "Create new directory with current date"
  (interactive)
  (dired-create-directory (format-time-string "%Y-%m-%d")))

(defvar file-extensions-with-default-apps '("xls" "doc" "xlsx" "docx" "eps" "dwg" "psd" "drawio" "pptx")
  "List of file extensions to open with default applications.")

(defvar video-file-extensions '("mp4" "mov" "webm" "mkv"))

(defvar html-file '("html"))

(defun my/eww-html-file ()
  (let* ((file (dired-get-filename)))
    (eww (concat "file://" file))))

(defun open-with-default-app ()
  "Open file with system default app in dired."
  (interactive)
  (let* ((file (dired-get-filename))
         (ext (file-name-extension file)))
    (cond ((member ext file-extensions-with-default-apps)
           (call-process "open"
                         nil 0 nil
                         (expand-file-name (dired-get-filename))))
          ((member ext video-file-extensions)
		   (my/dired-open-with-mpv))
		  ((member ext html-file)
		   (my/eww-html-file))
          (t (dired-find-file)))))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "<return>") 'open-with-default-app))

(setopt dired-omit-verbose nil
		dired-omit-files "^\\.[^.].*")
(add-hook 'dired-mode-hook #'dired-omit-mode)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "s-.") #'dired-omit-mode)
  (define-key dired-mode-map (kbd "C-c i") #'image-dired)
  (define-key dired-mode-map (kbd "s-/ l") #'org-store-link))

(defun my/org-attach-visit-headline-from-dired ()
  "Go to the headline corresponding to this org-attach directory."
  (interactive)
  (require 'org-attach)
  (let* ((path (replace-regexp-in-string (regexp-quote org-attach-directory) "" (expand-file-name (dired-filename-at-point))))
         (id-parts (split-string path "/"))
         (id1 (nth 1 id-parts))
         (id2 (nth 2 id-parts))
         (id (concat id1 id2)))
    (let ((m (org-id-find id 'marker)))
      (unless m (user-error "Cannot find entry with ID \"%s\"" id))
      (pop-to-buffer (marker-buffer m))
      (goto-char m)
      (move-marker m nil)
      (org-fold-show-context))))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-'") #'my/org-attach-visit-headline-from-dired))

;; Preview file in Dired.
(when (eq system-type 'darwin)
  (defun my/dired-preview ()
	"Quick look the current file in macOS."
	(interactive)
	(let* ((file (dired-get-filename)))
      (call-process-shell-command (concat "qlmanage -p " (shell-quote-argument file)) nil nil)))

  (with-eval-after-load 'dired
	(define-key dired-mode-map (kbd "SPC") #'my/dired-preview)))

(use-package dired-preview
  :load-path "packages/dired-preview/"
  :commands dired-preview-mode
  :config
  (setq dired-preview-delay 0.0)
  (setq dired-preview-max-size (expt 2 20))

  (setq dired-preview-ignored-extensions-regexp
		(concat "\\."
				"\\(mkv\\|webm\\|mp4\\|mp3\\|ogg\\|m4a"
				"\\|gz\\|zst\\|tar\\|xz\\|rar\\|zip"
				"\\|png\\|jpg\\|jpeg"
				"\\|iso\\|epub\\|pdf\\)")))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "P") #'dired-preview-mode))

(defun eps-to-png-marked ()
  "Convert all marked EPS files in the current Dired buffer to PNG format using ImageMagick's convert utility.
Each input file is converted to a PNG file with the same basename.
This function requires ImageMagick's convert utility to be installed and available in the system's PATH."
  (interactive)
  (let ((eps-files (dired-get-marked-files)))
    (when (not eps-files)
      (error "No marked files in Dired buffer."))
    (let ((n 0))
      (message "Converting:\n")
      (dolist (epsfile eps-files)
        (let ((pngfile (concat (file-name-sans-extension epsfile) ".png")))
          (setq n (1+ n))
          (message "%d: %s to %s." n epsfile pngfile)
          (start-process "eps-to-png"
                         "*eps-to-png*"
                         "convert"
                         "-colorspace"
                         "sRGB"
                         "-density"
                         "600x600"
                         epsfile
                         pngfile)))
      (message "\n%d files were converted from EPS to PNG format." n))))

;; Diredfl
;; Diredfl is not compatible with denote-dired-mode
;; (use-package diredfl
;;   :load-path "packages/diredfl/"
;;   :hook ((dired-mode . diredfl-mode)
;; 		 (denote-dired-mode . (lambda ()
;; 								(setq-local diredfl-mode nil)))))

(use-package file-info
  :load-path "packages/file-info.el/" "packages/hydra/" "packages/browse-at-remote/"
  :bind ("C-c i" . 'file-info-show)
  :config
  (setq hydra-hint-display-type 'posframe)
  (setq hydra-posframe-show-params `(:poshandler posframe-poshandler-frame-center
												 :internal-border-width 2
												 :internal-border-color "#61AFEF"
												 :left-fringe 16
												 :right-fringe 16)))
(use-package consult-dir
  :load-path "packages/consult-dir/"
  :commands consult-dir)

(provide 'init-dired)
;;; init-dired.el ends here
