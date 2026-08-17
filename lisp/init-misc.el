;; -*- lexical-binding: t; -*-

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

(defun switch-to-message ()
  "Quick switch to `*Message*' buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(global-set-key (kbd "M-g m") #'switch-to-message)
(global-set-key (kbd "M-g s") #'scratch-buffer)

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

(defmacro my/define-search-functions ()
  "Dynamically define search functions for each search engine in `my/browser-engines`."
  `(progn
     ,@(mapcar (lambda (engine)
                 (let* ((engine-name (car engine))
                        (function-name (intern (format "my/search-%s" (downcase (symbol-name engine-name))))))
                   `(defun ,function-name (query)
                      ,(format "Search for QUERY using the %s engine." engine-name)
                      (interactive
                       (list (let ((default-query
                                    (if (and (eq system-type 'darwin)
                                             (featurep 'emt))
                                        (emt-word-at-point-or-forward)
                                      (thing-at-point 'word t))))
                               (if (region-active-p)
                                   (buffer-substring-no-properties (region-beginning) (region-end))
                                 (read-string (format "[%s] Enter search terms (default: %s): " ,(symbol-name engine-name) default-query))))))
                      (let ((search-url (concat ,(cdr engine) (url-encode-url query))))
                        (browse-url search-url))
                      (when (region-active-p)
                        (deactivate-mark)))))
               my/browser-engines)))

(add-hook 'after-init-hook (lambda ()
  			     (my/define-search-functions)))

(global-set-key (kbd "M-s g") #'my/search-google)
(global-set-key (kbd "M-s W") #'my/search-wikipedia_en)
(global-set-key (kbd "M-s z") #'my/search-zhihu)
(global-set-key (kbd "M-s m") #'my/search-doubanmovie)
(global-set-key (kbd "M-s b") #'my/search-doubanbook)
;; (global-set-key (kbd "M-s y") #'my/search-youtube)
(global-set-key (kbd "M-s s") #'my/search-scholar)
(global-set-key (kbd "M-s S") #'my/search-semanticscholar)

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

(global-set-key (kbd "C-c f g") #'generate-folder-tree)

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
(global-set-key (kbd "C-c f R") #'my/open-project-readme)


(provide 'init-misc)
