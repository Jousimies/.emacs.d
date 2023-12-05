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

(when (executable-find "gls")
    (setopt dired-use-ls-dired nil
			insert-directory-program "gls"
			dired-listing-switches
			"-l --almost-all --human-readable --group-directories-first --no-group"))

(setopt dired-dwim-target t
		dired-auto-revert-buffer #'dired-buffer-stale-p
		dired-recursive-copies 'always
		dired-recursive-deletes 'top
		dired-auto-revert-buffer t)

(global-set-key (kbd "C-x d") #'dired)

(defun z/dired-insert-date-folder ()
  "Create new directory with current date"
  (interactive)
  (dired-create-directory (format-time-string "%Y-%m-%d")))

(defun my/eww-html-file ()
  (interactive)
  (let* ((file (dired-get-filename)))
    (eww (concat "file://" file))))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c e") 'my/eww-html-file))

(defvar file-extensions-with-default-apps '("xls" "doc" "xlsx" "docx" "eps" "dwg" "psd" "drawio" "pptx")
  "List of file extensions to open with default applications.")

(defvar video-file-extensions '("mp4" "mov"))

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
          (t (dired-find-file)))))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "<return>") 'open-with-default-app))

(defun my/dired-preview ()
  "Quick look the current file in macOS."
  (interactive)
  (let* ((file (dired-get-filename)))
    (if (eq system-type 'darwin)
        (call-process-shell-command (concat "qlmanage -p " (shell-quote-argument file)) nil nil)
      (message "Not supported on this platform."))))

(add-hook 'dired-mode-hook
          (lambda () (setq-local truncate-lines t)))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-<return>") #'my/dired-preview))

(setopt dired-omit-verbose nil
		dired-omit-files "^\\.[^.].*")
(add-hook 'dired-mode-hook #'dired-omit-mode)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "s-.") #'dired-omit-mode)
  (define-key dired-mode-map (kbd "C-c i") #'image-dired)
  (define-key dired-mode-map (kbd "C-c l") #'org-store-link))

(defun xah-show-in-desktop ()
  "Show current file in desktop.
This command can be called when in a file buffer or in `dired'."
  (interactive)
  (let (($path (if (buffer-file-name) (buffer-file-name) default-directory)))
    (cond
     ((string-equal system-type "windows-nt")
      (shell-command
       (format "PowerShell -Command Start-Process Explorer -FilePath %s"
               (shell-quote-argument default-directory))))
     ((string-equal system-type "darwin")
      (if (eq major-mode 'dired-mode)
          (let (($files (dired-get-marked-files )))
            (if (eq (length $files) 0)
                (shell-command (concat "open " (shell-quote-argument (expand-file-name default-directory ))))
              (shell-command (concat "open -R " (shell-quote-argument (car (dired-get-marked-files )))))))
        (shell-command
         (concat "open -R " (shell-quote-argument $path)))))
     ((string-equal system-type "gnu/linux")
      (let ((process-connection-type nil)
            (openFileProgram (if (file-exists-p "/usr/bin/gvfs-open")
                                 "/usr/bin/gvfs-open"
                               "/usr/bin/xdg-open")))
        (start-process "" nil openFileProgram (shell-quote-argument $path)))))))

(global-set-key (kbd "C-c f d") 'xah-show-in-desktop)


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

;; dired-preview
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

(provide 'init-dired)
;;; init-dired.el ends here
