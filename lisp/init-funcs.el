;; -*- lexical-binding: t; -*-

;; init-buffer
;;;###autoload
(defun my/delete-trailing-whitespace-except-current-line ()
  "Delete trailing whitespace, but keep the current line intact."
  (interactive)
  (let ((beg (point-min))
        (end (point-max))
        (bol (line-beginning-position))
        (eol (line-end-position)))
    (delete-trailing-whitespace beg bol)
    (delete-trailing-whitespace eol end)))

;;;###autoload
(defun auto-save-delete-trailing-whitespace-except-current-line ()
    (interactive)
    (let ((begin (line-beginning-position))
          (end (point))
          (buffername (buffer-name (buffer-base-buffer))))
      (when (not (or (string-prefix-p "inbox" buffername)
                     (string-match-p "^[0-9]" buffername)))
        (save-excursion
          (when (< (point-min) begin)
            (save-restriction
              (narrow-to-region (point-min) (1- begin))
              (delete-trailing-whitespace)))
          (when (> (point-max) end)
            (save-restriction
              (narrow-to-region end (point-max))
              (delete-trailing-whitespace)))))))


;; init-dired
;;;###autoload
(defun z/dired-insert-date-folder ()
  "Create new directory with current date"
  (interactive)
  (dired-create-directory (format-time-string "%Y-%m-%d")))

;;;###autoload
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
      (org-fold-show-context))))

;; init-edit
;;;###autoload
(defun my/selected-wrap-textcolor (color)
    "用 \textcolor{COLOR}{region} 包裹选中的文字。"
    (interactive "sEnter color (default red): ")
    (let ((c (if (string-empty-p color) "red" color))
          (beg (region-beginning))
          (end (region-end)))
      (save-excursion
	(goto-char end)
	(insert "}")
	(goto-char beg)
	(insert (format "\\textcolor{%s}{" c)))))

;;;####autoload
(defun my/org-insert-emphasis-with-zws (marker)
    "在标记符两侧自动插入零宽空格 (U+200B) 并包裹内容。"
    (interactive "sEnter marker (e.g. *, ~, =. default =): ")
    (let* ((c (if (string-empty-p marker) "*" marker))
	   (zws "\u200b")
           (has-region (use-region-p))
           (beg (if has-region (region-beginning) (point)))
           (end (if has-region (region-end) (point))))
      (goto-char end)
      (insert c zws)
      (goto-char beg)
      (insert zws c)
      (if has-region
          (goto-char (+ end 4))
	(forward-char 2))))

;;;###autoload
(defun my/org-element-unwrap-emphasis ()
    "参照 jf/org-link-remove-link 的逻辑，精准删除标记符及两侧的零宽空格。"
    (interactive)
    (let ((elem (org-element-context))
          (zws ?\u200b))
      (when (memq (car elem) '(bold italic code verbatim strike-through underline))
	(let* ((begin (org-element-property :begin elem))
               (end (org-element-property :end elem))
               (marker-char (buffer-substring-no-properties begin (1+ begin)))
               (actual-beg begin)
               (actual-end end)
               content)
          (setq content
		(if (org-element-property :contents-begin elem)
                    (buffer-substring-no-properties
                     (org-element-property :contents-begin elem)
                     (org-element-property :contents-end elem))
                  (buffer-substring-no-properties (+ begin 1) (- end 1))))

          (when (eq (char-before begin) zws)
            (setq actual-beg (1- begin)))

          (if (eq (char-after end) zws)
              (setq actual-end (1+ end))
            (when (eq (char-before end) zws)
              (setq actual-end end)))

          (delete-region actual-beg actual-end)
          (insert content)
          (message "已清理标记: %s" marker-char)))))

;;;###autoload
(defun my/embark-symbol-overlay-toggle ()
      "如果当前符号未高亮，则高亮它；
如果当前符号已经处于高亮状态，则清除缓冲区内所有高亮。"
      (interactive)
      (if (get-char-property (point) 'symbol-overlay)
	  (progn
            (symbol-overlay-remove-all)
            (message "Cleared all highlights."))
	(symbol-overlay-put)))


;; init-git
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

(eval-and-compile
  (require 'transient nil t))

(when (featurep 'transient)
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


;; init-misc
;;;###autoload
(defun my/insert-specified-datetree ()
  "Insert a datetree entry for a specified date."
  (interactive)
  (let* ((date (org-parse-time-string (org-read-date)))
         (year (nth 5 date))
         (month (nth 4 date))
         (day (nth 3 date)))
    (org-datetree-find-date-create (list month day year))
    (open-line 1)
    (forward-line 1)))

;;;###autoload
(defun switch-to-message ()
  "Quick switch to `*Message*' buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))
