;; init-misc.el --- Misc. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package advance-words-count
  :load-path "packages/advance-words-count.el/"
  :bind ("M-=" . advance-words-count))

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

(defun jf/org-link-remove-link ()
  "Remove the link part of an `org-mode' link at point and keep only the description."
  (interactive)
  (let ((elem (org-element-context)))
    (when (eq (car elem) 'link)
      (let* ((content-begin (org-element-property :contents-begin elem))
             (content-end  (org-element-property :contents-end elem))
             (link-begin (org-element-property :begin elem))
             (link-end (org-element-property :end elem)))
        (when (and content-begin content-end)
          (let ((content (buffer-substring-no-properties content-begin content-end)))
            (delete-region link-begin link-end)
            (insert content)))))))
(global-set-key (kbd "C-c m r") 'jf/org-link-remove-link)

(defun yt-set-time (time)
  "Set TIME in the YouTube link at point.)
  TIME is number of seconds if called from Lisp, and a string if
  called interactively.
  Supported formats:
  - seconds
  - minutes:seconds
  - number of seconds with the \"s\" suffix."
  (interactive (list
                (if current-prefix-arg
                    (prefix-numeric-value current-prefix-arg)
                  (read-string "Time: "))))
  (let ((url (thing-at-point-url-at-point)))
    (if (and url
             (string-match
              (format "^%s"
                      (regexp-opt
                       '("https://www.youtube.com/"
                         "https://youtu.be/")
                       "\\(?:")))
             url))
    (let* ((bounds (thing-at-point-bounds-of-url-at-point))
           (time-present-p (string-match "t=[0-9]+" url))
           (question-mark-present-p (string-search "?" url))
           (seconds (cond
                     ((numberp time)
                      time)
                     ((string-match
                       "^\\([0-9]+\\):\\([0-9]\\{2\\}\\)$" time)
                      (+ (* 60 (string-to-number
                                (match-string 1 time)))
                         (string-to-number (match-string 2 time))))
                     ((string-match "^\\([0-9]+\\)s?$" time)
                      (string-to-number (match-string 1 time)))
                     (t (error "Wrong argument format"))))
           (new-url (if time-present-p
                        (replace-regexp-in-string
                         "t=[0-9]+"
                         (format "t=%i" seconds)
                         url)
                      (concat url
                              (if question-mark-present-p "&" "?")
                              (format "t=%i" seconds)))))
      (delete-region (car bounds) (cdr bounds))
      (insert new-url))
    (error "Not on a Youtube link")))

(defun my/ocr ()
"OCR with Macos system."
  (interactive)
  (shell-command "shortcuts run \"OCR Selected Area\"")
  (do-applescript "tell application id \"org.gnu.Emacs\" to activate"))
(global-set-key (kbd "C-c m o") 'my/ocr)

(use-package mpv
  :commands mpv-start
  :load-path "packages/mpv.el/")

;;;###autoload
(defun my/mpv-play-at-point ()
  (interactive)
  (mpv-start (dired-get-filename)))

(use-package emt
  :load-path "packages/emt"
  :bind (("M-f" . emt-forward-word)
         ("M-b" . emt-backward-word)
         ("M-d" . emt-kill-word)
         ("M-h" . emt-backward-kill-word))
  :config
  (emt-ensure))

(provide 'init-misc)
;;; init-misc.el ends here.
