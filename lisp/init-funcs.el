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

;; init-completion
;;;###autoload
(defun my/ensure-orderless-before-completion ()
  "Load Orderless if the minibuffer wins the race with idle preloading."
  (require 'orderless)
  (remove-hook 'minibuffer-setup-hook
               #'my/ensure-orderless-before-completion))

;;;###autoload
(defun toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

;; browser
(setopt my/browser-engines
        '((DoubanMovie . "https://search.douban.com/movie/subject_search?search_text=")
          (DoubanBook . "https://search.douban.com/book/subject_search?search_text=")
          (Zhihu . "https://www.zhihu.com/search?type=content&q=")
          (Google . "https://www.google.com/search?q=")
          (Scholar . "https://scholar.google.com/scholar?q=")
          (SemanticScholar . "https://www.semanticscholar.org/search?q=")
          (Github . "https://github.com/search?q=")
          (Youtube . "http://www.youtube.com/results?aq=f&oq=&search_query=")
  	  (Bilibili . "https://search.bilibili.com/all?keyword=")
  	  (WikiPedia_en . "https://en.wikipedia.org/w/index.php?search=")
  	  (Annas-Archvie . "https://annas-archive.org/search?q=")))

(defun my/search-web (engine query)
  "Search QUERY using ENGINE from `my/browser-engines'."
  (let ((base-url (alist-get engine my/browser-engines)))
    (unless base-url
      (user-error "Unknown search engine: %s" engine))
    (browse-url (concat base-url (url-encode-url query))))
  (when (region-active-p)
    (deactivate-mark)))

(defun my/search-read-query (engine)
  "Read search query for ENGINE."
  (let ((default-query (thing-at-point 'word t)))
    (if (region-active-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (read-string (format "[%s] Search: " engine) default-query))))

;;;###autoload
(defun my/search-google (query)
  (interactive (list (my/search-read-query 'Google)))
  (my/search-web 'Google query))

;;;###autoload
(defun my/search-wikipedia_en (query)
  (interactive (list (my/search-read-query 'WikiPedia_en)))
  (my/search-web 'WikiPedia_en query))

;;;###autoload
(defun my/search-zhihu (query)
  (interactive (list (my/search-read-query 'Zhihu)))
  (my/search-web 'Zhihu query))

;;;###autoload
(defun my/search-doubanmovie (query)
  (interactive (list (my/search-read-query 'DoubanMovie)))
  (my/search-web 'DoubanMovie query))

;;;###autoload
(defun my/search-doubanbook (query)
  (interactive (list (my/search-read-query 'DoubanBook)))
  (my/search-web 'DoubanBook query))

;;;###autoload
(defun my/search-scholar (query)
  (interactive (list (my/search-read-query 'Scholar)))
  (my/search-web 'Scholar query))

;;;###autoload
(defun my/search-semanticscholar (query)
  (interactive (list (my/search-read-query 'SemanticScholar)))
  (my/search-web 'SemanticScholar query))

(global-set-key (kbd "M-s g") #'my/search-google)
(global-set-key (kbd "M-s W") #'my/search-wikipedia_en)
(global-set-key (kbd "M-s z") #'my/search-zhihu)
(global-set-key (kbd "M-s m") #'my/search-doubanmovie)
(global-set-key (kbd "M-s b") #'my/search-doubanbook)
;; (global-set-key (kbd "M-s y") #'my/search-youtube)
(global-set-key (kbd "M-s s") #'my/search-scholar)
(global-set-key (kbd "M-s S") #'my/search-semanticscholar)

;;;###autoload
(defun jf/org-link-remove-link ()
  "Remove the link part of an `org-mode' link at point and keep only the description."
  (interactive)
  (let ((elem (org-element-context)))
    (when (eq (car elem) 'link)
      (let* ((content-begin (org-element-property :contents-begin elem))
             (content-end  (org-element-property :contents-end elem))
             (link-begin (org-element-property :begin elem))
             (link-end (+ content-end 2)))   ; skip closing "]]", avoid eating trailing space
        (when (and content-begin content-end)
          (let ((content (buffer-substring-no-properties content-begin content-end)))
            (delete-region link-begin link-end)
            (insert content)))))))

(global-set-key (kbd "C-c l r") #'jf/org-link-remove-link)

(defvar folder-structure-new
  '((:name "00_设计依据-方案-地勘" :subfolders ("01_设计说明" "02_甲方提供资料"))
    (:name "01_结构工程-施工图" :subfolders ("01_施工图-提资" "02_施工图_设计"))
    (:name "02_结构工程-计算模型" :subfolders ())
    (:name "03_结构工程-计算书" :subfolders ())
    (:name "04_审图" :subfolders ("01_审图意见" "02_施工图_审图修改"))
    (:name "05_施工配合" :subfolders ())
    (:name "06_图纸归档" :subfolders ("01_施工图_终版" "02_计算模型_终版" "02_计算书_终版"))
    (:name "07_参考资料" :subfolders ())
    )
  "预定义的文件夹树结构。")

(defvar folder-structure-reinforcement
  '((:name "00_检测鉴定报告" :subfolders ("01_测绘图纸" "02_检测鉴定报告" "03_现场照片"))
    (:name "01_加固设计文件" :subfolders ("01_方案设计" "02_初步设计" "03_施工图设计" "04_设计变更"))
    (:name "02_结构计算模型" :subfolders ("01_原结构模型" "02_加固方案模型" "03_最终模型"))
    (:name "03_计算书" :subfolders ("01_承载力验算" "02_抗震验算" "03_加固节点计算" "04_专家评审"))
    (:name "04_施工图文件" :subfolders ("01_施工图提资" "02_施工图_设计"))
    (:name "05_审图记录" :subfolders ("01_审图意见" "02_审图修改"))
    (:name "06_施工配合" :subfolders ())
    (:name "07_工程档案归档" :subfolders ("01_施工图" "02_加固模型" "03_计算书" "04_验收证书"))
    (:name "08_参考资料" :subfolders ()))
  "加固项目文件夹结构")

(defun create-folder-structure (base-path structure-type)
  "在指定路径下生成文件夹树结构。
BASE-PATH: 基础路径
STRUCTURE-TYPE: 结构类型，:new 或 :reinforcement"
  (let ((folder-structure (cond ((eq structure-type :new) folder-structure-new)
				((eq structure-type :reinforcement) folder-structure-reinforcement)
				(t (error "Invalid structure type: %s" structure-type)))))
    (dolist (folder folder-structure)
      (let* ((folder-name (plist-get folder :name))
             (subfolders (plist-get folder :subfolders))
             (parent-path (expand-file-name folder-name base-path)))
        (create-folder parent-path)
        (create-subfolders parent-path subfolders)))))

(defun create-folder (path)
  "创建指定路径的文件夹。"
  (unless (file-exists-p path)
    (make-directory path t)
    (message "创建文件夹: %s" path)))

(defun create-subfolders (parent-path subfolders)
  "在父文件夹路径下创建子文件夹。"
  (dolist (folder subfolders)
    (let ((subfolder-path (expand-file-name folder parent-path)))
      (create-folder subfolder-path))))

;;;###autoload
(defun generate-folder-tree ()
  "生成以日期和标题命名的文件夹，并在其中创建Readme文件。"
  (interactive)
  (let* ((current-date (format-time-string "%Y%m%d"))
         (title (read-string "请输入标题: "))
         ;; 使用 completing-read-multiple 输入多个 tag，用 _ 连接
         (tags (mapconcat #'identity
                          (completing-read-multiple
                           "请输入标签（多个标签用逗号或空格分隔）: "
                           nil nil nil nil nil nil)
                          "_"))
         (folder-name (concat current-date "==" title (if tags (concat "_" tags) "")))
         (base-path (expand-file-name folder-name my/project-folder))
         ;; 选择文件夹结构类型
         (structure-type-string (completing-read "选择文件夹结构类型: "
                                                 '("new" "reinforcement")
                                                 nil t nil nil "new"))
         ;; 转换为关键字
         (structure-type (if (string= structure-type-string "new")
                             :new
                           :reinforcement)))

    ;; 创建文件夹
    (create-folder-structure base-path structure-type)

    ;; 创建Readme文件
    (with-temp-file (expand-file-name "Readme.org" base-path)
      (insert (format "#+TITLE: %s\n#+TAGS: %s\n#+DATE: %s\n\n\n"
                      title
                      (if tags tags "nil")
                      current-date
                      )))))

(defun my/project-readme-candidates ()
  (let ((root my/project-folder)
        files)
    (when (file-directory-p root)
      (dolist (dir (directory-files root t directory-files-no-dot-files-regexp))
        (when (file-directory-p dir)
          (let ((readme (expand-file-name "Readme.org" dir)))
            (when (file-regular-p readme)
              (push readme files))))))
    (nreverse files)))

;;;###autoload
(defun my/open-project-readme ()
  (interactive)
  (let* ((files (my/project-readme-candidates))
         (table (mapcar (lambda (f)
                          (cons (file-name-nondirectory
                                 (directory-file-name (file-name-directory f)))
                                f))
                        files))
         (name (consult--read table
                              :prompt "Project Readme: "
                              :require-match t
                              :category 'file
                              :sort nil)))
    (find-file (cdr (assoc name table)))))
