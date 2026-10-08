;; -*- lexical-binding: t; -*-

(require 'org)
(require 'seq)
(require 'subr-x)

;;;; 基本设置

(defgroup my/structural-project nil
  "结构设计项目目录管理。"
  :group 'files)

(defcustom my/project-folder
  (expand-file-name "~/Projects/")
  "结构设计项目的根目录。

例如：

  D:/3-Resource/结构项目/

建议使用正斜杠，也可以使用 Windows 反斜杠。"
  :type 'directory
  :group 'my/structural-project)

(defcustom my/project-readme-name "Readme.org"
  "每个项目根目录中的说明文件名。"
  :type 'string
  :group 'my/structural-project)

;;;; 新建项目目录结构

(defvar my/project-structure-new
  '((:name "00_项目管理"
     :subfolders
     ("01_合同与任务书"
      "02_进度与人员"
      "03_会议纪要"
      "04_收发文记录"))

    (:name "01_设计输入"
     :subfolders
     ("01_甲方资料"
      "02_建筑条件"
      "03_机电与设备条件"
      "04_地勘资料"
      "05_荷载与工艺条件"
      "06_其他专业条件"))

    (:name "02_方案与前期资料"
     :subfolders
     ("01_结构方案"
      "02_方案比较"
      "03_初步设计"))

    (:name "03_结构设计文件"
     :subfolders
     ("01_结构对外提资"
      "02_施工图工作文件"
      "03_校审文件"
      "04_设计输出"))

    (:name "04_计算模型"
     :subfolders
     ("01_试算模型"
      "02_正式模型"
      "03_专项模型"
      "04_审查修改模型"))

    (:name "05_计算书"
     :subfolders
     ("01_整体计算"
      "02_构件与节点计算"
      "03_专项计算"
      "04_计算附件"))

    (:name "06_审查与报审"
     :subfolders
     ("01_送审文件"
      "02_审查意见"
      "03_审查回复"
      "04_审查修改"))

    (:name "07_施工配合"
     :subfolders
     ("01_图纸会审与交底"
      "02_现场问题"
      "03_设计变更"
      "04_技术核定"
      "05_验收配合"))

    (:name "08_成果归档"
     :subfolders
     ("01_施工图"
      "02_计算模型"
      "03_计算书"
      "04_审查文件"
      "05_设计变更"))

    (:name "09_参考资料"
     :subfolders ()))
  "新建结构工程项目的标准目录。")

;;;; 加固项目目录结构

(defvar my/project-structure-reinforcement
  '((:name "00_项目管理"
     :subfolders
     ("01_合同与任务书"
      "02_进度与人员"
      "03_会议纪要"
      "04_收发文记录"))

    (:name "01_原结构资料"
     :subfolders
     ("01_原设计图纸"
      "02_竣工图纸"
      "03_历史变更资料"
      "04_使用与维修记录"))

    (:name "02_检测鉴定"
     :subfolders
     ("01_现场测绘"
      "02_现场照片"
      "03_检测资料"
      "04_鉴定报告"))

    (:name "03_加固方案"
     :subfolders
     ("01_方案设计"
      "02_方案比较"
      "03_专家论证"))

    (:name "04_加固设计文件"
     :subfolders
     ("01_结构对外提资"
      "02_施工图工作文件"
      "03_校审文件"
      "04_设计输出"))

    (:name "05_计算模型"
     :subfolders
     ("01_原结构模型"
      "02_加固方案模型"
      "03_专项模型"
      "04_审查修改模型"))

    (:name "06_计算书"
     :subfolders
     ("01_原结构验算"
      "02_加固后验算"
      "03_构件与节点计算"
      "04_专项计算"))

    (:name "07_审查与报审"
     :subfolders
     ("01_送审文件"
      "02_审查意见"
      "03_审查回复"
      "04_审查修改"))

    (:name "08_施工配合"
     :subfolders
     ("01_图纸会审与交底"
      "02_现场问题"
      "03_设计变更"
      "04_技术核定"
      "05_验收配合"))

    (:name "09_成果归档"
     :subfolders
     ("01_施工图"
      "02_计算模型"
      "03_计算书"
      "04_检测鉴定"
      "05_审查与论证"
      "06_设计变更"
      "07_验收资料"))

    (:name "10_参考资料"
     :subfolders ()))
  "既有结构检测、鉴定与加固项目的标准目录。")

;;;; 协作项目目录结构

(defvar my/project-structure-collaboration
  '((:name "00_收到资料" :subfolders ())
    (:name "01_工作文件" :subfolders ())
    (:name "02_提交成果" :subfolders ()))
  "非全程参与的协作项目精简目录。")

;;;; 辅助函数

(defconst my/project-types
  '(("新建结构工程" . :new)
    ("既有结构鉴定与加固" . :reinforcement)
    ("协作项目" . :collaboration))
  "项目类型显示名称与内部关键字的对应表。")

(defun my/project-sanitize-file-name (name)
  "替换 NAME 中不允许出现在 Windows 文件名中的字符。"
  (let ((result (replace-regexp-in-string
                 "[<>:\"/\\\\|?*]" "_" (string-trim name))))
    ;; Windows 文件名不能以空格或句点结尾。
    (setq result (replace-regexp-in-string "[ .]+\\'" "" result))
    (when (string-empty-p result)
      (user-error "名称不能为空"))
    result))

(defun my/project-create-folder (path)
  "创建 PATH 文件夹。

如果文件夹已经存在，则不做修改。"
  (unless (file-directory-p path)
    (make-directory path t)
    (message "创建文件夹：%s" path)))

(defun my/project-create-subfolders (parent-path subfolders)
  "在 PARENT-PATH 中创建 SUBFOLDERS。"
  (dolist (folder subfolders)
    (my/project-create-folder (expand-file-name folder parent-path))))

(defun my/project-get-structure (structure-type)
  "根据 STRUCTURE-TYPE 返回项目目录结构。"
  (pcase structure-type
    (:new my/project-structure-new)
    (:reinforcement my/project-structure-reinforcement)
    (:collaboration my/project-structure-collaboration)
    (_ (error "无效的项目类型：%s" structure-type))))

(defun my/project-create-folder-structure (base-path structure-type)
  "在 BASE-PATH 中创建 STRUCTURE-TYPE 对应的目录结构。"
  (dolist (folder (my/project-get-structure structure-type))
    (let* ((folder-name (plist-get folder :name))
           (subfolders (plist-get folder :subfolders))
           (parent-path (expand-file-name folder-name base-path)))
      (my/project-create-folder parent-path)
      (my/project-create-subfolders parent-path subfolders))))

(defun my/project-read-tags ()
  "读取用户输入的项目标签并返回标签列表。

支持使用逗号、中文逗号、顿号、分号或空格分隔。"
  (let ((input (read-string "请输入标签，多个标签用逗号或空格分隔：")))
    (if (string-empty-p (string-trim input))
        nil
      (delete-dups
       (mapcar #'my/project-sanitize-file-name
               (split-string input "[[:space:],，、;；]+" t))))))

(defun my/project-tags-for-folder-name (tags)
  "把 TAGS 转换为下划线分隔的项目文件夹名称片段。"
  (if tags
      (concat "_" (string-join tags "_"))
    ""))

(defun my/project-tags-for-org (tags)
  "把 TAGS 转换为 Org FILETAGS 格式。"
  (if tags
      (concat ":" (string-join tags ":") ":")
    ""))

(defun my/project-type-name (structure-type)
  "返回 STRUCTURE-TYPE 的中文名称。"
  (or (car (rassq structure-type my/project-types)) "未知类型"))

(defun my/project-read-structure-type ()
  "读取并返回用户选择的项目类型。"
  (let ((choice (completing-read
                 "请选择项目类型：" my/project-types
                 nil t nil nil (caar my/project-types))))
    (alist-get choice my/project-types nil nil #'string=)))

(defun my/project-write-readme
    (base-path title tags current-date structure-type)
  "在 BASE-PATH 中创建项目 Readme.org。

如果 Readme.org 已存在，则不覆盖。"
  (let ((readme (expand-file-name my/project-readme-name base-path))
        (project-type-name (my/project-type-name structure-type)))
    (if (file-exists-p readme)
        (message "Readme 已存在，未覆盖：%s" readme)
      (with-temp-file readme
        (set-buffer-file-coding-system 'utf-8)
        (insert "#+title: " title "\n"
                "#+date: " current-date "\n"
                "#+category: 结构设计项目\n"
                "#+project_type: " project-type-name "\n")
        (when tags
          (insert "#+filetags: " (my/project-tags-for-org tags) "\n"))
        (let* ((project-path (expand-file-name base-path))
               (project-link
                (org-link-make-string
                 (concat "file:" (org-link-escape project-path))
                 project-path)))
          (insert "#+startup: overview\n\n"
                  "* 项目信息\n\n"
                  "- 项目名称 :: " title "\n"
                  "- 项目类型 :: " project-type-name "\n"
                  "- 建立日期 :: " current-date "\n"
                  "- 项目路径 :: " project-link "\n"))
        (when tags
          (insert "- 项目标签 :: " (string-join tags "、") "\n"))
        (insert "\n"
                "* 项目说明\n\n"
                "* 当前进展\n\n"
                "* 待办事项\n\n"
                "* 重要决定\n\n"
                "* 文件索引\n\n"))
      (message "创建 Readme：%s" readme))))

;;;; 生成项目目录

;;;###autoload
(defun my/generate-structural-project ()
  "生成结构设计项目文件夹和 Readme.org。"
  (interactive)
  (unless (and my/project-folder
               (not (string-empty-p my/project-folder)))
    (user-error "请先设置 my/project-folder"))
  (let* ((current-date (format-time-string "%Y%m%d"))
         (title (my/project-sanitize-file-name
                 (read-string "请输入项目标题：")))
         (tags (my/project-read-tags))
         (structure-type (my/project-read-structure-type))
         ;; 使用下划线连接标签，生成 Windows 友好的目录名。
         (folder-name (concat current-date "==" title
                              (my/project-tags-for-folder-name tags)))
         (base-path (expand-file-name folder-name my/project-folder)))
    ;; 确保项目根目录存在。
    (my/project-create-folder base-path)
    ;; 创建项目目录结构。
    (my/project-create-folder-structure base-path structure-type)
    ;; 创建项目说明文件。
    (my/project-write-readme base-path title tags current-date structure-type)
    ;; 打开项目 Readme。
    (find-file (expand-file-name my/project-readme-name base-path))
    (message "项目目录创建完成：%s" base-path)))

;;;; 查询和打开项目

(defun my/project-display-name (readme)
  "从 README 路径获得项目显示名称。"
  (file-name-nondirectory
   (directory-file-name (file-name-directory readme))))

(defun my/project-readme-candidates ()
  "返回项目根目录下所有一级项目的 Readme.org。"
  (let ((root (expand-file-name my/project-folder))
        files)
    (when (file-directory-p root)
      (dolist (directory
               (directory-files root t directory-files-no-dot-files-regexp))
        (when (file-directory-p directory)
          (let ((readme (expand-file-name my/project-readme-name directory)))
            (when (file-regular-p readme)
              (push readme files))))))
    (sort files
          (lambda (a b)
            (string-lessp (my/project-display-name a)
                          (my/project-display-name b))))))

(defun my/project-read-project (prompt)
  "使用 PROMPT 让用户选择项目，并返回其 Readme 路径。"
  (let ((files (my/project-readme-candidates)))
    (unless files
      (user-error "在项目根目录中没有找到 %s：%s"
                  my/project-readme-name my/project-folder))
    (let* ((table (mapcar (lambda (file)
                            (cons (my/project-display-name file) file))
                          files))
           (selection (completing-read prompt table nil t)))
      (or (cdr (assoc-string selection table t))
          (user-error "没有找到项目：%s" selection)))))

;;;###autoload
(defun my/open-project-readme ()
  "从项目列表中选择并打开 Readme.org。"
  (interactive)
  (find-file (my/project-read-project "选择结构设计项目：")))

;;;###autoload
(defun my/open-project-directory ()
  "选择一个结构设计项目并打开其根目录。"
  (interactive)
  (dired (file-name-directory
          (my/project-read-project "选择项目目录："))))

(provide 'init-funcs-proj)
