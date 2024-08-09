;; init-misc.el --- Misc. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:


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

(use-package snow
  :load-path "~/.emacs.d/packages/snow.el/"
  :commands snow)

;; (use-package focus
;;   :load-path "packages/Focus/"
;;   :hook (prog-mode . focus-mode))

;; (use-package zoom
;;   :load-path "packages/zoom/"
;;   :hook (window-setup . zoom-mode)
;;   :config
;;   (defun size-callback ()
;; 	(cond ((> (frame-pixel-width) 1280) '(90 . 0.75))
;;           (t                            '(0.5 . 0.5))))
;;   (setopt zoom-size #'size-callback)
;;   (setopt zoom-ignored-buffer-name-regexps '("^*calc"))
;;   (setopt zoom-ignored-major-modes '(telega-root-mode telega-chat-mode)))

(defun my/wallpaper-set ()
  (interactive)
  (wallpaper-set (buffer-file-name)))

;; 没啥用
(defgroup init-lock nil
  "Lock files behind simple arithmetic."
  :group 'convenience
  :prefix "init-lock")

(defcustom init-lock-files nil
  "Files to lock behind arithmetic."
  :type '(repeat file))

(defun init-lock-loop ()
  (let ((answer 1)
        (input)
        (a)
        (b))
    (while (not (eq answer input))
      (setq a (random 1000))
      (setq b (random 100))
      (setq answer (- a b))
      (setq input (read-number (format "%s - %s = " a b))))
    nil))

;;;###autoload
(defun init-lock (orig-fun &rest args)
  (when (member (car args) init-lock-files)
    (init-lock-loop))
  (apply orig-fun args))

;;;###autoload
(defun init-lock-enable ()
  (interactive)
  (advice-add 'find-file :around 'init-lock))

(defun init-lock-disable ()
  (interactive)
  (init-lock-loop)
  (advice-remove 'find-file 'file-lock))

;; Sometimes I want swith a theme.
(defun my/switch-theme ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (let* ((themes (if (eq ns-system-appearance 'light)
                     ef-themes-light-themes
				   love/dark-themes))
         (theme (elt themes (random (length themes)))))
    (load-theme theme t)))

(provide 'init-misc)
;;; init-misc.el ends here.
