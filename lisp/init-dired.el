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

(use-package dired
  :if (and IS-MAC
	   (executable-find "gls"))
  :custom
  (dired-use-ls-dired nil)
  (insert-directory-program "gls")
  (dired-listing-switches
   "-l --almost-all --human-readable --group-directories-first --no-group"))

(use-package dired
  :bind (:map dired-mode-map
	      ("C-'" . my/org-attach-visit-headline-from-dired))
  :hook (dired-mode . (lambda ()
			(setq-local truncate-lines t)))
  :custom
  (dired-dwim-target t)
  (dired-auto-revert-buffer #'dired-buffer-stale-p)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-auto-revert-buffer t)
  (dired-filename-display-length 'window))

(with-eval-after-load 'dired
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
	(org-fold-show-context)))))

(use-package dired-x
  :ensure nil
  :hook ((dired-mode . dired-omit-mode)
	 (dired-mode . dired-hide-details-mode))
  :bind (:map dired-mode-map
	      ("s-." . dired-omit-mode)
	      ("C-c i" . image-dired)
	      ("s-/ l" . org-store-link))
  :custom
  (dired-omit-verbose nil)
  (dired-omit-files "^\\.[^.].*"))

(use-package dired-aux
  :ensure nil
  :after dired
  :custom
  (dired-isearch-filenames 'dwim)
  (dired-create-destination-dirs 'ask)
  (dired-vc-rename-file t)
  (dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir))))
  (dired-create-destination-dirs-on-trailing-dirsep t))

;; dired-do-shell-command, open file with default application.
;; (let ((cmd (cond ((and (eq system-type 'darwin) (display-graphic-p)) "open")
;;                  ((and (eq system-type 'gnu/linux) (display-graphic-p)) "xdg-open")
;;                  ((and (eq system-type 'windows-nt) (display-graphic-p)) "start")
;;                  (t ""))))
;;   (setq dired-guess-shell-alist-user
;;         `(("\\.\\(?:docx\\|doc\\|xlsx\\|xls\\|ppt\\|pptx\\)\\'" ,cmd)
;; 	  ("\\.\\(?:eps\\|dwg\\|psd\\|drawio\\)\\'" ,cmd)
;;           ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
;;           ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
;;           ("\\.\\(?:xcf\\)\\'" ,cmd)
;; 	  ("\\.\\(?:epub\\|pdf\\)\\'" ,cmd)
;;           ("\\.csv\\'" ,cmd)
;;           ("\\.tex\\'" ,cmd)
;;           ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
;;           ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd))))

;; (defun z/dired-insert-date-folder ()
;;   "Create new directory with current date"
;;   (interactive)
;;   (dired-create-directory (format-time-string "%Y-%m-%d")))

;; (defvar file-extensions-with-default-apps '("xls" "doc" "xlsx" "docx" "eps" "dwg" "psd" "drawio" "pptx")
;;   "List of file extensions to open with default applications.")

;; (defvar video-file-extensions '("mp4" "mov" "webm" "mkv" "mp3"))

;; (defvar html-file '("html"))

;; (defun my/eww-html-file ()
;;   (let* ((file (dired-get-filename)))
;;     (eww (concat "file://" file))))

;; (defun open-with-default-app ()
;;   "Open file with system default app in dired."
;;   (interactive)
;;   (let* ((file (dired-get-filename))
;;          (ext (file-name-extension file)))
;;     (cond ((member ext file-extensions-with-default-apps)
;;            (call-process "open"
;;                          nil 0 nil
;;                          (expand-file-name (dired-get-filename))))
;; 	  ((member ext html-file)
;; 	   (my/eww-html-file))
;;           (t (dired-find-file)))))

;; (with-eval-after-load 'dired
;;   (define-key dired-mode-map (kbd "<return>") 'open-with-default-app))

;; Preview file in Dired.
(when (eq system-type 'darwin)
  (defun my/dired-preview ()
    "Quick look the current file in macOS."
    (interactive)
    (let* ((file (dired-get-filename)))
      (call-process-shell-command (concat "qlmanage -p " (shell-quote-argument file)) nil nil)))

  (with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "SPC") #'my/dired-preview)))

;; (use-package nerd-icons-dired
;;   :load-path "~/.emacs.d/packages/nerd-icons-completion/"
;;   :hook (dired-mode . nerd-icons-dired-mode))

(use-package dired-preview
  :load-path "packages/dired-preview/"
  :commands dired-preview-mode
  :custom
  (dired-preview-delay 0.0)
  (dired-preview-max-size (expt 2 20))
  (dired-preview-ignored-extensions-regexp
   (concat "\\."
	   "\\(mkv\\|webm\\|mp4\\|mp3\\|ogg\\|m4a"
	   "\\|gz\\|zst\\|tar\\|xz\\|rar\\|zip"
	   ;; "\\|png\\|jpg\\|jpeg"
	   "\\|iso\\|epub\\|pdf\\)")))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "P") #'dired-preview-mode))

;; (defun eps-to-png-marked ()
;;   "Convert all marked EPS files in the current Dired buffer to PNG format using ImageMagick's convert utility.
;; Each input file is converted to a PNG file with the same basename.
;; This function requires ImageMagick's convert utility to be installed and available in the system's PATH."
;;   (interactive)
;;   (let ((eps-files (dired-get-marked-files)))
;;     (when (not eps-files)
;;       (error "No marked files in Dired buffer."))
;;     (let ((n 0))
;;       (message "Converting:\n")
;;       (dolist (epsfile eps-files)
;;         (let ((pngfile (concat (file-name-sans-extension epsfile) ".png")))
;;           (setq n (1+ n))
;;           (message "%d: %s to %s." n epsfile pngfile)
;;           (start-process "eps-to-png"
;;                          "*eps-to-png*"
;;                          "convert"
;;                          "-colorspace"
;;                          "sRGB"
;;                          "-density"
;;                          "600x600"
;;                          epsfile
;;                          pngfile)))
;;       (message "\n%d files were converted from EPS to PNG format." n))))

;; (use-package dired-async
;;   :hook (dired-mode . dired-async-mode))

;; https://mbork.pl/2024-02-17_Opening_external_drives_in_Dired
;; (defcustom automount-directory (format "/run/media/%s" user-login-name)
;;   "Directory under which drives are automounted.")

;; (defun automount-open-in-dired ()
;;   "Open the automounted drive in Dired.
;; If there is more than one, let the user choose."
;;   (interactive)
;;   (let ((dirs (directory-files automount-directory nil "^[^.]")))
;;     (dired (file-name-concat
;;             automount-directory
;;             (cond ((null dirs)
;;                    (error "No drives mounted at the moment"))
;;                   ((= (length dirs) 1)
;;                    (car dirs))
;;                   (t
;;                    (completing-read "Open in dired: " dirs nil t)))))))

(use-package consult-dir
  :load-path "packages/consult-dir/"
  :bind (([remap dired] . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package dired-sidebar
  :load-path "packages/dired-sidebar/" "packages/dired-hacks/"
  :bind ("C-x C-n" . dired-sidebar-toggle-sidebar)
  :custom
  (dired-sidebar-mode-line-format '("%e" my/winum "丨" mode-line-front-space mode-line-buffer-identification " " mode-line-end-spaces)))

;; Enhancing Dired Sorting With Transient
;; http://yummymelon.com/devnull/enhancing-dired-sorting-with-transient.html

(defun cc/--dired-sort-by (criteria &optional prefix-args)
  "Sort current Dired buffer according to CRITERIA and PREFIX-ARGS.

This function will invoke `dired-sort-other' with arguments built from
CRITERIA and PREFIX-ARGS.

CRITERIA is a keyword of which the following are supported:
  :name             :date-added             :version
  :kind             :date-metadata-changed  :size
  :date-last-opened :date-modified

PREFIX-ARGS is a list of GNU ls arguments. If nil, then it will use the value
of `cc-dired-listing-switches'. Otherwise this is typically populated by the
Transient menu `cc/dired-sort-by'.

This function requires GNU ls from coreutils installed.

See the man page `ls(1)' for details."
  (let ((arg-list (list "-l")))
    (if prefix-args
        (nconc arg-list prefix-args)
      (nconc arg-list cc-dired-listing-switches))
    (cond
     ((eq criteria :name)
      (message "Sorted by name"))

     ((eq criteria :kind)
      (message "Sorted by kind")
      (push "--sort=extension" arg-list))

     ((eq criteria :date-last-opened)
      (message "Sorted by date last opened")
      (push "--sort=time" arg-list)
      (push "--time=access" arg-list))

     ((eq criteria :date-added)
      (message "Sorted by date added")
      (push "--sort=time" arg-list)
      (push "--time=creation" arg-list))

     ((eq criteria :date-modified)
      (message "Sorted by date modified")
      (push "--sort=time" arg-list)
      (push "--time=modification" arg-list))

     ((eq criteria :date-metadata-changed)
      (message "Sorted by date metadata changed")
      (push "--sort=time" arg-list)
      (push "--time=status" arg-list))

     ((eq criteria :version)
      (message "Sorted by version")
      (push "--sort=version" arg-list))

     ((eq criteria :size)
      (message "Sorted by size")
      (push "-S" arg-list))

     (t
      (message "Default sorted by name")))

    (dired-sort-other (mapconcat 'identity arg-list " "))))

(with-eval-after-load 'transient
  (transient-define-prefix cc/dired-sort-by ()
    "Transient menu to sort Dired buffer by different criteria.

This function requires GNU ls from coreutils installed."
    :value '("--human-readable"
             "--group-directories-first"
             "--time-style=long-iso")
					; TODO: support cc-dired-listing-switches
    [["Arguments"
      ("-a" "all" "--all")
      ("g" "group directories first" "--group-directories-first")
      ("-r" "reverse" "--reverse")
      ("-h" "human readable" "--human-readable")
      ("t" "time style" "--time-style="
       :choices ("full-iso" "long-iso" "iso" "locale"))]

     ["Sort By"
      ("n"
       "Name"
       (lambda () (interactive)
	 (cc/--dired-sort-by :name
                             (transient-args transient-current-command)))
       :transient nil)
      ("k"
       "Kind"
       (lambda () (interactive)
	 (cc/--dired-sort-by :kind
                             (transient-args transient-current-command)))
       :transient nil)
      ("l"
       "Date Last Opened"
       (lambda () (interactive)
	 (cc/--dired-sort-by :date-last-opened
                             (transient-args transient-current-command)))
       :transient nil)
      ("a"
       "Date Added"
       (lambda () (interactive)
	 (cc/--dired-sort-by :date-added
                             (transient-args transient-current-command)))
       :transient nil)
      ("m"
       "Date Modified"
       (lambda () (interactive)
	 (cc/--dired-sort-by :date-modified
                             (transient-args transient-current-command)))
       :transient nil)
      ("M"
       "Date Metadata Changed"
       (lambda () (interactive)
	 (cc/--dired-sort-by :date-metadata-changed
                             (transient-args transient-current-command)))
       :transient nil)
      ("v"
       "Version"
       (lambda () (interactive)
	 (cc/--dired-sort-by :version
                             (transient-args transient-current-command)))
       :transient nil)
      ("s"
       "Size"
       (lambda () (interactive)
	 (cc/--dired-sort-by :size
                             (transient-args transient-current-command)))
       :transient nil)]]))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "s") #'cc/dired-sort-by))

(provide 'init-dired)
;;; init-dired.el ends here
