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

(defvar file-extensions-with-default-apps '("xls" "doc" "xlsx" "docx" "eps" "dwg" "psd" "drawio")
  "List of file extensions to open with default applications.")

(defun open-with-default-app ()
  "Open file with system default app in dired."
  (interactive)
  (let* ((file (dired-get-filename))
         (ext (file-name-extension file)))
    (cond ((member ext file-extensions-with-default-apps)
           (call-process "open"
                         nil 0 nil
                         (expand-file-name (dired-get-filename))))
          ((equal ext "mp4") (my/mpv-play-at-point))
          (t (dired-find-file)))))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "<return>") 'open-with-default-app))

(defun dired-preview ()
  "Quick look the current file in macOS."
  (interactive)
  (let* ((file (dired-get-filename)))
    (if (eq system-type 'darwin)
        (call-process-shell-command (concat "qlmanage -p " (shell-quote-argument file)) nil nil)
      (message "Not supported on this platform."))))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-<return>") #'dired-preview))

(setopt dired-omit-verbose nil
		dired-omit-files "^\\.[^.].*")
(add-hook 'dired-mode-hook #'dired-omit-mode)
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


(provide 'init-dired)
;;; init-dired.el ends here
