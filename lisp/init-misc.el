;; init-misc.el --- Misc. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

;; Adjust alpha background
(defun lucius/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha-background) 100))
         ;; The 'alpha frame param became a pair at some point in
         ;; emacs 24.x, e.g. (100 100)
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha-background newalpha))))))

(defun my/increase-alpha-background ()
  (interactive)
  (lucius/adjust-opacity (selected-frame) 5))

(defun my/decrease-alpha-background ()
  (interactive)
  (lucius/adjust-opacity (selected-frame) -5))

(global-set-key (kbd "C-<f1>") #'my/decrease-alpha-background)
(global-set-key (kbd "C-<f2>") #'my/increase-alpha-background)

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

(use-package snow)

(use-package 2048-game)

(use-package sudoku)

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

(use-package gptel
  :bind ("<f1>" . gptel)
  :config
  (setq gptel-model "deepseek-chat"
        gptel-backend
        (gptel-make-openai "DeepSeek"
          :host "api.deepseek.com"
          :endpoint "/chat/completions"
          :stream t
          :key (auth-source-pick-first-password
                :host "deepseek"
                :user "apikey")
          :models '("deepseek-chat" "deepseek-coder"))))


(provide 'init-misc)
;;; init-misc.el ends here.
