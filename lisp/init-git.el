;; -*- lexical-binding: t; -*-

(use-package browse-at-remote
  :bind ("M-g b" . browse-at-remote))

(use-package magit
  :bind ("C-x g" . magit)
  :config
  (magit-add-section-hook 'magit-status-sections-hook
			  'magit-insert-modules
			  'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-module-sections-hook 'magit-insert-modules-overview)
  (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpulled-from-pushremote)
  (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpushed-to-pushremote)
  ;; (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpushed-to-upstream)
  )


;;; Git Submodule management (Emacs-native)

(defgroup my/git-submodule nil
  "Manage packages as git submodules under packages/."
  :group 'tools)

(defcustom my/git-submodule-packages-dir
  (expand-file-name "packages" user-emacs-directory)
  "Directory where submodules live."
  :type 'directory
  :group 'my/git-submodule)

(defcustom my/git-submodule-depth 1
  "Default --depth for git submodule add/update."
  :type 'integer
  :group 'my/git-submodule)

(defvar my/git-submodule-buffer "*git-submodule*"
  "Buffer name for submodule command output.")

(defun my/git-submodule--run (args &optional async)
  "Run git submodule with ARGS.
If ASYNC is non-nil, use async-shell-command."
  (let ((default-directory user-emacs-directory)
        (cmd (mapconcat #'shell-quote-argument
                        (cons "git" (cons "submodule" args))
                        " ")))
    (if async
        (async-shell-command cmd my/git-submodule-buffer)
      (with-current-buffer (get-buffer-create my/git-submodule-buffer)
        (erase-buffer)
        (let ((status (apply #'call-process "git" nil t nil
                             (cons "submodule" args))))
          (if (eq status 0)
              (progn
                (message "OK: git submodule %s" (car args))
                (when (get-buffer-window my/git-submodule-buffer)
                  (kill-buffer my/git-submodule-buffer)))
            (pop-to-buffer my/git-submodule-buffer)
            (user-error "git submodule %s failed (exit %s)" (car args) status))
          status)))))

(defun my/git-submodule--repo-name-from-url (url)
  "Extract directory name from URL."
  (replace-regexp-in-string
   "\\.git\\'" ""
   (file-name-nondirectory (directory-file-name url))))

(defun my/git-submodule--list-packages ()
  "Return list of package directory names under packages/."
  (when (file-directory-p my/git-submodule-packages-dir)
    (cl-remove-if
     (lambda (name)
       (or (string-prefix-p "." name)
           (not (file-directory-p
                 (expand-file-name name my/git-submodule-packages-dir)))))
     (directory-files my/git-submodule-packages-dir))))

(defun my/git-submodule--read-package (&optional prompt)
  "Read a package name with completion."
  (completing-read (or prompt "Package: ")
                   (my/git-submodule--list-packages)
                   nil t))


;;; Commands

;;;###autoload
(defun my/git-submodule-add (url &optional name)
  "Add URL as a git submodule under packages/ with --depth.
NAME is the directory name; if nil, derive from URL."
  (interactive
   (list (read-string "Git URL: ")
         (let ((n (read-string "Directory name (empty = from URL): ")))
           (unless (string-empty-p n) n))))
  (let* ((repo-name (or name (my/git-submodule--repo-name-from-url url)))
         (target (expand-file-name repo-name my/git-submodule-packages-dir))
         (rel (file-relative-name target user-emacs-directory)))
    (unless (file-directory-p my/git-submodule-packages-dir)
      (make-directory my/git-submodule-packages-dir t))
    (when (file-exists-p target)
      (user-error "Already exists: %s" target))
    (message "Adding submodule %s -> %s ..." url rel)
    (my/git-submodule--run
     (list "add" "--depth" (number-to-string my/git-submodule-depth)
           url rel))
    ;; Optional: refresh load-path immediately
    (add-to-list 'load-path target t)
    (message "Done: %s (you may want to commit .gitmodules)" rel)))

;;;###autoload
(defun my/git-submodule-update (&optional remote)
  "Update all submodules (init + update).
With prefix REMOTE, also fetch remote updates (`--remote`)."
  (interactive "P")
  (let ((args (append '("update" "--init" "--recursive")
                      (when remote '("--remote"))
                      (when my/git-submodule-depth
                        (list "--depth" (number-to-string my/git-submodule-depth))))))
    (message "Updating submodules%s..." (if remote " (remote)" ""))
    (my/git-submodule--run args t)))  ; async, may take time

;;;###autoload
(defun my/git-submodule-update-one (package &optional remote)
  "Update a single PACKAGE submodule.
With prefix REMOTE, use `--remote`."
  (interactive
   (list (my/git-submodule--read-package "Update package: ")
         current-prefix-arg))
  (let* ((path (expand-file-name package my/git-submodule-packages-dir))
         (rel (file-relative-name path user-emacs-directory))
         (args (append '("update" "--init")
                       (when remote '("--remote"))
                       (list rel))))
    (message "Updating %s..." package)
    (my/git-submodule--run args)))

;;;###autoload
(defun my/git-submodule-status ()
  "Show git submodule status."
  (interactive)
  (my/git-submodule--run '("status" "--recursive") t))

;;;###autoload
(defun my/git-submodule-sync ()
  "Sync submodule URLs from .gitmodules."
  (interactive)
  (my/git-submodule--run '("sync" "--recursive")))

;;;###autoload
(defun my/git-submodule-remove (package)
  "Remove PACKAGE submodule completely (deinit + rm + clean modules)."
  (interactive (list (my/git-submodule--read-package "Remove package: ")))
  (when (yes-or-no-p (format "Really remove submodule packages/%s? " package))
    (let* ((path (expand-file-name package my/git-submodule-packages-dir))
           (rel (file-relative-name path user-emacs-directory))
           (default-directory user-emacs-directory))
      ;; deinit
      (call-process "git" nil my/git-submodule-buffer t
                    "submodule" "deinit" "-f" rel)
      ;; rm from index and working tree
      (call-process "git" nil my/git-submodule-buffer t
                    "rm" "-f" rel)
      ;; clean .git/modules
      (let ((mod (expand-file-name
                  (concat "modules/" rel) (expand-file-name ".git" user-emacs-directory))))
        (when (file-exists-p mod)
          (delete-directory mod t)))
      (message "Removed packages/%s (commit the change when ready)" package))))

;;;###autoload
(defun my/git-submodule-foreach-pull ()
  "Run `git pull` in every submodule (async)."
  (interactive)
  (my/git-submodule--run '("foreach" "--recursive" "git pull") t))

;;;###autoload
(defun my/git-submodule-open (package)
  "Open the PACKAGE directory in dired."
  (interactive (list (my/git-submodule--read-package "Open package: ")))
  (dired (expand-file-name package my/git-submodule-packages-dir)))

;;;###autoload
(defun my/git-submodule-browse-remote (package)
  "Browse the remote of PACKAGE (requires browse-at-remote or similar)."
  (interactive (list (my/git-submodule--read-package "Browse package: ")))
  (let ((default-directory
         (expand-file-name package my/git-submodule-packages-dir)))
    (if (fboundp 'browse-at-remote)
        (browse-at-remote)
      (user-error "browse-at-remote not available"))))


;;; Optional: transient menu

(when (require 'transient nil t)
  (transient-define-prefix my/git-submodule-menu ()
    "Git Submodule management"
    [["Add / Remove"
      ("a" "Add"          my/git-submodule-add)
      ("r" "Remove"       my/git-submodule-remove)]
     ["Update"
      ("u" "Update all"   my/git-submodule-update)
      ("U" "Update remote" (lambda () (interactive) (my/git-submodule-update t)))
      ("o" "Update one"   my/git-submodule-update-one)
      ("p" "foreach pull" my/git-submodule-foreach-pull)]
     ["Info"
      ("s" "Status"       my/git-submodule-status)
      ("y" "Sync URLs"    my/git-submodule-sync)
      ("d" "Open dir"     my/git-submodule-open)
      ("b" "Browse remote" my/git-submodule-browse-remote)]]))

;; Example binding (adjust as you like)
;; (global-set-key (kbd "C-c g s") #'my/git-submodule-menu)

;;; Fix incomplete .gitmodules (Windows Compatible)

(defun my/git-submodule--normalize-path (path)
  "Convert PATH to relative posix style (e.g., packages/foo)."
  (when path
    (setq path (file-relative-name path user-emacs-directory))
    (file-name-as-directory-less (replace-regexp-in-string "\\\\" "/" path))))

(defun my/git-submodule--gitlinks ()
  "Return alist of (PATH . COMMIT) for all gitlinks under packages/."
  (let ((default-directory user-emacs-directory)
        (result nil))
    (with-temp-buffer
      ;; 使用 --error-unmatch 或确保 process 正常
      (when (zerop (call-process "git" nil t nil "ls-files" "--stage"))
        (goto-char (point-min))
        (while (re-search-forward "^160000 \\([0-9a-f]+\\) [0-9]\t\\(packages/[^\r\n]+\\)$" nil t)
          (let ((commit (match-string 1))
                (path (match-string 2)))
            (push (cons (replace-regexp-in-string "\\\\" "/" path) commit) result)))))
    (nreverse result)))

(defun my/git-submodule--registered-paths ()
  "Return list of paths already recorded in .gitmodules."
  (let ((file (expand-file-name ".gitmodules" user-emacs-directory))
        paths)
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "^\\s-*path\\s-*=\\s-*\\(.+\\)$" nil t)
          (let ((p (string-trim (match-string 1))))
            ;; 过滤掉 Windows 换行符 \r 并统一转为正斜杠 /
            (setq p (replace-regexp-in-string "\r" "" p))
            (setq p (replace-regexp-in-string "\\\\" "/" p))
            (push p paths)))))
    paths))

(defun my/git-submodule-missing ()
  "Return list of package paths that are gitlinks but missing from .gitmodules."
  (let ((gitlinks (mapcar #'car (my/git-submodule--gitlinks)))
        (registered (my/git-submodule--registered-paths)))
    (cl-set-difference gitlinks registered :test #'string=)))

;;;###autoload
(defun my/git-submodule-list-missing ()
  "Show packages that are gitlinks but not in .gitmodules."
  (interactive)
  (let ((missing (my/git-submodule-missing)))
    (if (null missing)
        (message "All gitlinks are registered in .gitmodules.")
      (with-current-buffer (get-buffer-create "*Submodule Missing*")
        (read-only-mode -1)
        (erase-buffer)
        (insert "以下包是 gitlink，但未登记在 .gitmodules 中：\n\n")
        (dolist (p missing)
          (insert (format "  %s\n" p)))
        (insert "\n用 M-x my/git-submodule-fix-one 或 my/git-submodule-fix-all 补全。\n")
        (goto-char (point-min))
        (pop-to-buffer (current-buffer))))))

(defun my/git-submodule--guess-url (path)
  "Try to guess remote URL from an existing package directory."
  (let ((default-directory (expand-file-name path user-emacs-directory)))
    (when (file-directory-p default-directory)
      (with-temp-buffer
        (when (zerop (call-process "git" nil t nil "remote" "get-url" "origin"))
          (replace-regexp-in-string "[\r\n]" "" (buffer-string)))))))

;;;###autoload
(defun my/git-submodule-fix-one (path &optional url)
  "Register PATH (e.g. packages/consult) into .gitmodules using URL.
If URL is nil, try to guess from the package's own remote, then prompt."
  (interactive
   (let* ((missing (my/git-submodule-missing))
          (path (completing-read "Fix package: " missing nil t)))
     (list path)))
  (unless (string-prefix-p "packages/" path)
    (setq path (concat "packages/" path)))
  (let* ((full (expand-file-name path user-emacs-directory))
         (guess (or url (my/git-submodule--guess-url path)))
         (url (or url
                  (read-string (format "URL for %s: " path)
                               (or guess "")))))
    (when (string-empty-p url)
      (user-error "URL required"))
    (unless (file-directory-p full)
      (user-error "Directory does not exist: %s" full))
    (let ((default-directory user-emacs-directory))
      ;; --force 允许在已有 gitlink 的情况下补写 .gitmodules
      (let ((status
             (call-process "git" nil my/git-submodule-buffer t
                           "submodule" "add" "--force"
                           "--depth" (number-to-string my/git-submodule-depth)
                           url path)))
        (if (eq status 0)
            (message "Registered: %s -> %s" path url)
          (pop-to-buffer my/git-submodule-buffer)
          (user-error "Failed to register %s (exit %s)" path status))))))

;;;###autoload
(defun my/git-submodule-fix-all ()
  "Interactively fix all missing .gitmodules entries."
  (interactive)
  (let ((missing (my/git-submodule-missing)))
    (if (null missing)
        (message "Nothing to fix.")
      (dolist (path missing)
        (condition-case err
              (my/git-submodule-fix-one path)
            (error (message "Skip %s: %s" path err))))
      (message "Done. 请检查并提交 .gitmodules"))))

(provide 'init-git)
