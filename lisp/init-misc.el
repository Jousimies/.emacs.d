;; init-misc.el --- Misc. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(defun toggle-proxy ()
  "Toggle proxy for the url.el library."
  (interactive)
  (if url-proxy-services
      (proxy-disable)
    (proxy-enable)))

(defun proxy-enable ()
  "Enable proxy."
  (interactive)
  (setq url-proxy-services
        '(("http" . "127.0.0.1:8118")
          ("https" . "127.0.0.1:8118")
          ("socks" . "127.0.0.1:8118")
          ("no_proxy" . "0.0.0.0")))
  (message "Proxy enabled! %s" (car url-proxy-services)))

(defun proxy-disable ()
  "Disable proxy."
  (interactive)
  (if url-proxy-services
      (setq url-proxy-services nil))
  (message "Proxy disabled!"))

(run-with-idle-timer 2 nil (lambda ()
                             (proxy-enable)))

(use-package advance-words-count
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

(provide 'init-misc)
;;; init-misc.el ends here.
