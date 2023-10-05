;; init-dired.el --- File manager. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package dired
  :commands dired-find-file
  :bind (("C-x d" . dired)
         (:map dired-mode-map
               ("C-c l" . org-store-link)))
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (when (executable-find "gls")

    (setq dired-use-ls-dired nil)
    (setq insert-directory-program "gls")
    (setq dired-listing-switches
          "-l --almost-all --human-readable --group-directories-first --no-group"))
  (setq dired-dwim-target t)
  (setq dired-auto-revert-buffer #'dired-buffer-stale-p)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'top)
  (setq dired-auto-revert-buffer t))

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

  ;;;###autoload
  (defun open-with-default-app ()
    "Open file with system default app in dired."
    (interactive)
    (let* ((file (dired-get-filename))
           (ext (file-name-extension file)))
      (if (member ext file-extensions-with-default-apps)
          (browse-url-of-dired-file)
        (dired-find-file))))

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

(use-package dired-x
  :hook (dired-mode . dired-omit-mode)
  :bind (:map dired-mode-map
              ("s-." . dired-omit-mode))
  :config
  (setq dired-omit-verbose nil)
  (setq dired-omit-files "^\\.[^.].*"))

(use-package image-dired
  :bind (:map dired-mode-map
              ("C-c l" . image-dired)))

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
;;; init-dired.el ends here.
