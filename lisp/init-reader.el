;; init-reader.el --- Reader. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package pdf-tools
  :straight t
  :magic ("%PDF" . pdf-view-mode)
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :hook ((pdf-tools-enabled . pdf-view-themed-minor-mode)
         (pdf-view-mode . (lambda ()
                            (require 'saveplace-pdf-view)))
	 (pdf-view-mode . pdf-occur-global-minor-mode)
	 (pdf-view-mode . pdf-history-minor-mode)
	 (pdf-view-mode . pdf-links-minor-mode)
	 (pdf-view-mode . pdf-outline-minor-mode)
	 (pdf-view-mode . pdf-annot-minor-mode)
	 (pdf-view-mode . pdf-sync-minor-mode))
  :bind ((:map pdf-outline-buffer-mode-map
               ("RET" . pdf-outline-follow-link-and-quit))
	 (:map pdf-annot-edit-contents-minor-mode-map
              ("<return>" . pdf-annot-edit-contents-commit)
              ("<S-return>" . newline)))
  :custom
  (pdf-view-display-size 'fit-width)
  (pdf-view-use-unicode-ligther nil)
  (pdf-view-use-scaling t)
  (pdf-view-use-imagemagick nil)
  (pdf-annot-activate-created-annotations nil)
  (pdf-misc-print-program-executable "/usr/bin/lp")
  :config
  (pdf-tools-install t nil t nil)
  (define-pdf-cache-function pagelabels)
  (defun mrb/pdf-misc-print-pages(filename pages &optional interactive-p)
    "Wrapper for `pdf-misc-print-document` to add page selection support."
    (interactive (list (pdf-view-buffer-file-name)
                       (read-string "Page range (empty for all pages): "
                                    (number-to-string (pdf-view-current-page)))
                       t) pdf-view-mode)
    (let ((pdf-misc-print-program-args
           (if (not (string-blank-p pages))
               (cons (concat "-P " pages) pdf-misc-print-program-args)
             pdf-misc-print-program-args)))
      (pdf-misc-print-document filename))))

(with-eval-after-load 'pdf-view
  (define-key pdf-view-mode-map [remap pdf-misc-print-document] #'mrb/pdf-misc-print-pages))

(use-package saveplace-pdf-view
  :straight t
  :defer t)

(use-package nov
  :straight t
  :mode (".epub" . nov-mode)
  :custom
  (nov-unzip-program (executable-find "bsdtar"))
  (nov-unzip-args '("-xC" directory "-f" filename))
  (nov-save-place-file (expand-file-name "nov_place" cache-directory)))

;; https://gist.github.com/krisbalintona/f4554bb8e53c27c246ae5e3c4ff9b342
;;;###autoload
(defun krisb-pdf-tools-metadata-bookmark-section ()
  "Insert bookmark metadata section."
  (interactive)
  (save-excursion
    (insert "\nBookmarkBegin\nBookmarkTitle: \nBookmarkLevel: 1\nBookmarkPageNumber: "))
  (move-end-of-line 2))

(defvar-keymap krisb-pdf-tools-metadata-mode-map
  :doc "Mode map for `krisb-pdf-tools-metadata-mode'."
  "C-c C-b" #'krisb-pdf-tools-metadata-bookmark-section)

(define-derived-mode krisb-pdf-tools-metadata-mode fundamental-mode "Metadata"
  "Major mode for altering and viewing PDF metadata."
  :interactive t
  (use-local-map krisb-pdf-tools-metadata-mode-map))

;;;###autoload
(defun krisb-pdf-tools-metadata-modify (pdf-file)
  "Modify PDF-FILE metadata."
  (interactive (list (buffer-file-name)))
  (unless (string= "pdf" (file-name-extension pdf-file))
    (user-error "File is not a PDF!"))
  (unless (executable-find "pdftk")
    (error "System executable `pdftk' not found. Please install executable on filesystem to proceed"))
  (let* ((pdf-name (file-name-sans-extension (file-name-nondirectory pdf-file)))
         (buf-name (concat "*pdf-tools metadata: " pdf-name))
         (metadata-file (concat "/tmp/pdf-tools-metadata--" pdf-name))
         (temp-pdf (make-temp-file "/tmp/pdf-tools-metadata--temp-pdf"))
         (metadata-dump-command (concat "pdftk \"" pdf-file "\" dump_data"))
         (metadata-update-command
          (concat "pdftk \"" pdf-file "\" update_info \"" metadata-file "\" output \"" temp-pdf "\""))
         (commit-func (lambda ()
                        "Commit the changes to PDF metadata."
                        (interactive)
                        (with-current-buffer buf-name
                          (widen)
                          (write-region (point-min) (point-max) metadata-file))
                        (shell-command metadata-update-command "*pdf-tools metadata: CLI output")
                        (kill-buffer buf-name)
                        ;; Have to do it this way since `pdftk' does not allow
                        ;; having the output file be the input file
                        (rename-file temp-pdf pdf-file t)
                        (message "Updated metadata!"))))
    (save-buffer)
    (with-current-buffer (get-buffer-create buf-name)
      (insert (shell-command-to-string metadata-dump-command))
      (goto-char (point-min))
      (krisb-pdf-tools-metadata-mode))
    (pop-to-buffer buf-name)
    (define-key krisb-pdf-tools-metadata-mode-map (kbd "C-c C-c") commit-func)
    (set-buffer-modified-p nil)
    (message (substitute-command-keys "Press `C-c C-c' when finished editing PDF metadata. To see keybinds, press \\[describe-mode]"))))


(provide 'init-reader)
;;; init-reader.el ends here.
