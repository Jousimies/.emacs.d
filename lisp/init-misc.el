(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

(use-package epkg
  :general (my/space-leader-def
             ".p" '(epkg-describe-package :wk "Package Search"))
  :config
  (setq epkg-repository (expand-file-name "cache/epkgs" user-emacs-directory)))

(use-package epkg-marginalia
  :after (epkg marginalia)
  :config
  (cl-pushnew 'epkg-marginalia-annotate-package
              (alist-get 'package marginalia-annotator-registry)))

(use-package gcmh
  :hook ((after-init . gcmh-mode)
         (focus-out . garbage-collect))
  :config
  (setq gcmh-idle-delay 'auto)
  (setq gcmh-auto-idle-delay-factor 10)
  (setq gcmh-high-cons-threshold #x1000000))

(use-package grab-mac-link
  :commands grab-mac-link-dwim grab-mac-link-safari-1
  :preface
  (defun my/link-grab ()
    (interactive)
    (grab-mac-link-dwim 'safari))
  :bind ("<f8>" . my/link-grab))

(use-package file-info
  :commands file-info-show
  :config
  (setq hydra-hint-display-type 'posframe)
  (setq hydra-posframe-show-params `(:poshandler posframe-poshandler-frame-center
                                                 :internal-border-width 2
                                                 :internal-border-color "#61AFEF"
                                                 :left-fringe 16
                                                 :right-fringe 16)))

(my/space-leader-def
  "mi" '(file-info-show :wk "File info"))

(use-package disk-usage
  :general (my/space-leader-def
             "m" '(:ignore t :wk "Misc")
             "md" '(disk-usage :wk "Disk usage")))

(use-package youtube-dl
  :commands youtube-dl
  :config
  (setq youtube-dl-directory "~/Downloads/")
  (setq youtube-dl-program "/opt/homebrew/bin/youtube-dl")
  (setq youtube-dl-arguments
        '("--no-mtime" "--restrict-filenames" "--format" "best" "--mark-watched")))

(defun my/ocr ()
"OCR with Macos system."
  (interactive)
  (shell-command "shortcuts run \"OCR Selected Area\"")
  (do-applescript "tell application id \"org.gnu.Emacs\" to activate"))

(my/space-leader-def
  "mo" '(my/ocr :wk "OCR"))

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

(my/space-leader-def
  "mp" '(toggle-proxy :wk "Proxy"))

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

(defun mac-launchpad/string-ends-with (s ending)
  "Return non-nil if string S ends with ENDING."
  (cond ((>= (length s) (length ending))
         (let ((elength (length ending)))
           (string= (substring s (- 0 elength)) ending)))
        (t nil)))

(defun mac-launchpad/find-mac-apps (folder)
  (let* ((files (directory-files folder))
         (without-dots (cl-delete-if (lambda (f) (or (string= "." f) (string= ".." f))) files))
         (all-files (mapcar (lambda (f) (file-name-as-directory (concat (file-name-as-directory folder) f))) without-dots))
         (result (cl-delete-if-not (lambda (s) (mac-launchpad/string-ends-with s ".app/")) all-files)))
    result))

(defun mac-launchpad ()
  (interactive)
  (let* ((apps (mac-launchpad/find-mac-apps "/Applications"))
         (to-launch (completing-read "launch: " apps)))
    (shell-command (format "defaults read \"%s\"Contents/Info.plist CFBundleIdentifier | xargs open -b" to-launch))))

(my/space-leader-def
  "mj" '(mac-launchpad :wk "Jump to App"))

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

(my/space-leader-def
  "of" '(xah-show-in-desktop :wk "Open Finder"))

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

(with-eval-after-load 'evil
  (evil-declare-key 'normal 'global
    "gX" 'jf/org-link-remove-link))

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

(my/space-leader-def
  "my" '(yt-set-time :wk "Youtube link time"))

(defun switch-to-message ()
  "Quick switch to `*Message*' buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun switch-to-scratch ()
  "Quick switch to `*Scratch*' buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(my/space-leader-def
  "bs" '(switch-to-scratch :wk "*scratch*")
  "bm" '(switch-to-message :wk "*message*"))

(defun my/inbox-file ()
  "Open inbox file."
  (interactive)
  (find-file (expand-file-name "inbox/inbox.org" my-galaxy)))

(defun my/plan-file ()
  "Open plan file."
  (interactive)
  (find-file (expand-file-name "inbox/plan.org" my-galaxy)))

(defun my/index-file ()
  (interactive)
  (find-file (expand-file-name "roam/main/index.org" my-galaxy)))

(defun my/reflection-file ()
  (interactive)
  (find-file (expand-file-name "roam/main/reflection.org" my-galaxy)))

(defun my/finance-file ()
  "Open finance file."
  (interactive)
  (find-file (expand-file-name "finance/beans/finance.bean" my-galaxy)))

(defun my/reading-record ()
  "Open reading record file."
  (interactive)
  (find-file (expand-file-name "denote/books/20230301T211439--Book-lists-and-reading-record__reading.org" my-galaxy)))

(my/space-leader-def
  "f" '(:ignore t :wk "Open files")
  "fb" '(my/reading-record :wk "Reading record")
  "fi" '(my/inbox-file :wk "Inbox file")
  "fI" '(my/index-file :wk "Index file")
  "fp" '(my/plan-file :wk "Plan file")
  "ff" '(my/finance-file :wk "Finance file")
  "fR" '(my/reflection-file :wk "Reflection file")
  "fg" '(my/gtd-file :wk "GTD file"))

(defun my/start-server ()
  (interactive)
  (if (not (server-running-p))
      (server-start))
  (message "Server has started"))

(defun my/scroll-other-windown-down ()
  "Scroll other window down."
  (interactive)
  (scroll-other-window-down 2))

(global-set-key (kbd "M-p") 'my/scroll-other-windown-down)

(defun my/scroll-other-windown ()
  "Scroll other window up."
  (interactive)
  (scroll-other-window 2))

(global-set-key (kbd "M-n") 'my/scroll-other-windown)

(use-package mind-wave
  :mode ("chat" . mind-wave-chat-mode))

(use-package org-ai
  :commands (org-ai-mode)
  :hook (org-mode . org-ai-mode)
  :config
  (org-ai-install-yasnippets))

(use-package which-key
  :hook (on-first-input . which-key-mode)
  :config
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 10000)
  (setq which-key-idle-secondary-delay 0.05))

(provide 'init-misc)
;;; init-misc.el ends here.
