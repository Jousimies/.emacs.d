;; init-misc.el --- Misc. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:
(use-package gptel
  :load-path "~/.emacs.d/packages/gptel/"
  :bind ("<f1>" . my/f1-toggle-gptel-buffer)
  :hook ((gptel-post-stream . gptel-auto-scroll)
	 (gptel-post-response . gptel-end-of-response))
  :custom
  (gptel--system-message (alist-get 'default gptel-directives))
  (gptel-default-mode 'org-mode)
  (gptel-directives
   `((default . "To assist:  Be terse.  Do not offer unprompted advice or clarifications.  Speak in specific,
 topic relevant terminology.  Do NOT hedge or qualify.  Speak directly and be willing to make creative guesses.

Explain your reasoning.  if you don’t know, say you don’t know.  Be willing to reference less reputable sources for
 ideas.  If you use LaTex notation, enclose math in \\( and \\), or \\[ and \\] delimiters.

 Never apologize.  Ask questions when unsure.")
     (programmer . "You are a careful programmer.  Provide code and only code as output without any additional text, prompt or note.  Do NOT use markdown backticks (```) to format your response.")
     (cliwhiz . "You are a command line helper.  Generate command line commands that do what is requested, without any additional description or explanation.  Generate ONLY the command, without any markdown code fences.")
     (emacser . "You are an Emacs maven.  Reply only with the most appropriate built-in Emacs command for the task I specify.  Do NOT generate any additional description or explanation.")
     (explain . "Explain what this code does to a novice programmer.")
     (tutor . "You are a tutor and domain expert in the domain of my questions.  You will lead me to discover the answer myself by providing hints.  Your instructions are as follows:
- If the question or notation is not clear to you, ask for clarifying details.
- At first your hints should be general and vague.
- If I fail to make progress, provide more explicit hints.
- Never provide the answer itself unless I explicitly ask you to.  If my answer is wrong, again provide only hints to correct it.
- If you use LaTeX notation, enclose math in \\( and \\) or \\[ and \\] delimiters.")
     ,@(let ((res))
         (pcase-dolist (`(,sym ,filename)
                        '((Autoexpert "detailed-prompt.md")
                          (writer "writer-prompt.md"))
                        res)
	   (when-let* ((big-prompt (locate-user-emacs-file filename))
                       (_ (file-exists-p big-prompt)))
	     (push
              `(,sym . ,(with-temp-buffer
                          (insert-file-contents big-prompt)
                          (goto-char (point-min))
                          (when (search-forward-regexp "^#" nil t)
			    (goto-char (match-beginning 0)))
                          (buffer-substring-no-properties (point) (point-max))))
              res)))
         res)))
  :config
  (defun my/f1-toggle-gptel-buffer ()
    "Toggle the *Gemini* buffer. If it exists, close it. Otherwise, call `gptel`."
    (interactive)
    (let ((gemini-buffer (get-buffer "*Gemini*")))
      (if (and gemini-buffer (get-buffer-window gemini-buffer))
          (progn
	    (kill-buffer gemini-buffer)
	    (delete-window))
	(call-interactively 'gptel)))))

(with-eval-after-load 'gptel
  (require 'gptel-curl)
  (setq gptel-model 'deepseek-chat)
  (setq gptel-backend
	(gptel-make-openai "DeepSeek"     ;Any name you want
          :host "api.deepseek.com"
          :endpoint "/chat/completions"
          :stream t
          :key (auth-source-pick-first-password
		:host "deepseek"
		:user "apikey")
          :models '(deepseek-chat deepseek-coder)))
  (require 'gptel-transient)
  (global-set-key (kbd "C-c C-<return>") #'gptel-menu)
  (require 'gptel-org)
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "*Prompt*: "
        (alist-get 'org-mode gptel-response-prefix-alist) "*Response*:\n"
        (alist-get 'markdown-mode gptel-prompt-prefix-alist) "#### ")
  (setq-default gptel-org-branching-context t)
  (require 'gptel-rewrite))

(use-package image-slicing
  :load-path "~/.emacs.d/packages/image-slicing/"
  :hook (org-mode . image-slicing-mode))

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

;; (use-package holo-layer
;;   :load-path "~/.emacs.d/packages/holo-layer/"
;;   :custom
;;   (holo-layer-python-command "~/.env/bin/python3"))

(use-package eee
  :load-path "~/.emacs.d/packages/eee.el/"
  :bind-keymap
  ("s-e" . ee-keymap)
  :custom
  (ee-terminal-command "/opt/homebrew/bin/wezterm"))

(defun switch-to-wezterm ()
    "Switch to WezTerm terminal."
    (interactive)
    (do-applescript "
    tell application \"WezTerm\"
      activate
    end tell"))

(advice-add 'ee-run :after
            (lambda (&rest _)
              (sleep-for 0.1)
              (switch-to-wezterm)))

(provide 'init-misc)
;;; init-misc.el ends here.
