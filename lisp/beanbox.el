;;; beanbox.el --- Run Beanbox from Emacs  -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Interactive commands for running Beanbox relative to one ledger root.
;; Set `beanbox-root-directory', then use `M-x beanbox' or one of:
;;
;;   M-x beanbox-start
;;   M-x beanbox-import
;;   M-x beanbox-review
;;   M-x beanbox-export
;;   M-x beanbox-stop

;;; Code:

(require 'compile)
(require 'project)
(require 'subr-x)

(defgroup beanbox nil
  "Run Beanbox commands from Emacs."
  :group 'tools)

(defcustom beanbox-root-directory nil
  "Root directory used to resolve all relative Beanbox paths.

For example, set this to the directory containing `config.toml',
`Sources', `Database', and `AllBeans'."
  :type '(choice (const :tag "Use current project/default directory" nil)
                 directory)
  :group 'beanbox)

(defcustom beanbox-program "beanbox"
  "Beanbox executable name or path.

If the command is installed only in this project's virtual environment,
set this to `.venv/bin/beanbox'."
  :type 'string
  :group 'beanbox)

(defcustom beanbox-config-file "config.toml"
  "Configuration file, relative to `beanbox-root-directory'."
  :type 'file
  :group 'beanbox)

(defcustom beanbox-database-directory "Database/"
  "Directory for platform-specific Inbox databases.

Each database is named after its platform, for example `alipay.db',
`wechat.db', or `cmb.db'."
  :type 'directory
  :group 'beanbox)

(defcustom beanbox-output-directory "AllBeans/"
  "Directory for platform-specific exported Beancount files.

Each output is named after its platform, for example `alipay.bean',
`wechat.bean', or `cmb.bean'."
  :type 'directory
  :group 'beanbox)

(defcustom beanbox-source-directory "Sources/"
  "Parent directory containing one subdirectory per platform.

The first directory below this path determines the platform.  For example,
`Sources/Alipay/statement.csv' uses platform `alipay'."
  :type 'directory
  :group 'beanbox)

(defcustom beanbox-platform-aliases
  '(("支付宝" . "alipay")
    ("微信" . "wechat")
    ("微信支付" . "wechat"))
  "Map source directory names to canonical platform names.

Names absent from this alist are normalized automatically, so a future
`Sources/CMB/' directory uses platform `cmb'."
  :type '(alist :key-type string :value-type string)
  :group 'beanbox)

(defcustom beanbox-review-host "127.0.0.1"
  "Host passed to `beanbox review'."
  :type 'string
  :group 'beanbox)

(defcustom beanbox-review-port 8765
  "Port passed to `beanbox review'."
  :type 'integer
  :group 'beanbox)

(defvar beanbox--server-process nil
  "The most recently started Beanbox Review server process.")

(defvar beanbox--last-platform nil
  "Most recently used platform in this Emacs session.")

(defun beanbox--root ()
  "Return the normalized Beanbox root directory."
  (file-name-as-directory
   (expand-file-name
    (or beanbox-root-directory
        (when-let* ((project (project-current nil)))
          (project-root project))
        default-directory))))

(defun beanbox--absolute (path)
  "Return absolute PATH, resolving it against the Beanbox root."
  (expand-file-name path (beanbox--root)))

(defun beanbox--relative (path)
  "Return PATH relative to the Beanbox root when it is below that root."
  (let* ((root (beanbox--root))
         (absolute (expand-file-name path root)))
    (if (file-in-directory-p absolute root)
        (file-relative-name absolute root)
      absolute)))

(defun beanbox--read-statement ()
  "Read a statement filename, starting in `beanbox-source-directory'."
  (beanbox--relative
   (read-file-name "账单文件: "
                   (beanbox--absolute beanbox-source-directory)
                   nil t)))

(defun beanbox--normalize-platform (name)
  "Return a safe, canonical platform identifier for NAME."
  (let* ((alias (cdr (assoc-string name beanbox-platform-aliases t)))
         (normalized
          (downcase
           (replace-regexp-in-string
            "[^[:alnum:]_-]+" "-" (or alias name))))
         (trimmed
          (replace-regexp-in-string
           "\\`[-_]+\\|[-_]+\\'" "" normalized)))
    (when (string-empty-p trimmed)
      (user-error "无法从 %S 推导平台名" name))
    trimmed))

(defun beanbox--platform-from-input (input)
  "Infer INPUT's platform from its first directory below `Sources/'."
  (let* ((source-root (beanbox--absolute beanbox-source-directory))
         (absolute (beanbox--absolute input))
         (relative (file-relative-name absolute source-root))
         (parts (split-string relative "/" t)))
    (when (or (file-name-absolute-p relative)
              (string-prefix-p "../" relative)
              (< (length parts) 2))
      (user-error
       "账单必须位于 %s<平台>/ 下，例如 Sources/Alipay/账单.csv"
       (beanbox--relative source-root)))
    (beanbox--normalize-platform (car parts))))

(defun beanbox--database-file (platform)
  "Return the database filename for PLATFORM."
  (expand-file-name (concat (beanbox--normalize-platform platform) ".db")
                    (beanbox--absolute beanbox-database-directory)))

(defun beanbox--output-file (platform)
  "Return the exported Beancount filename for PLATFORM."
  (expand-file-name (concat (beanbox--normalize-platform platform) ".bean")
                    (beanbox--absolute beanbox-output-directory)))

(defun beanbox--known-platforms ()
  "Return platforms found below Sources, Database, or AllBeans."
  (let (platforms)
    (let ((source (beanbox--absolute beanbox-source-directory)))
      (when (file-directory-p source)
        (dolist (entry (directory-files source t directory-files-no-dot-files-regexp))
          (when (file-directory-p entry)
            (push (beanbox--normalize-platform
                   (file-name-nondirectory (directory-file-name entry)))
                  platforms)))))
    (dolist (spec `((,beanbox-database-directory . "\\.db\\'")
                    (,beanbox-output-directory . "\\.bean\\'")))
      (let ((directory (beanbox--absolute (car spec))))
        (when (file-directory-p directory)
          (dolist (entry (directory-files directory nil (cdr spec)))
            (push (beanbox--normalize-platform (file-name-base entry))
                  platforms)))))
    (sort (delete-dups platforms) #'string-lessp)))

(defun beanbox--read-platform ()
  "Read a platform name, offering all platforms found on disk."
  (let ((platform
         (completing-read "平台: " (beanbox--known-platforms)
                          nil nil nil nil beanbox--last-platform)))
    (when (string-empty-p platform)
      (user-error "平台名不能为空"))
    (beanbox--normalize-platform platform)))

(defun beanbox--program ()
  "Return the executable used to launch Beanbox."
  (if (file-name-absolute-p beanbox-program)
      beanbox-program
    (let ((project-program (beanbox--absolute beanbox-program)))
      (if (file-executable-p project-program)
          project-program
        beanbox-program))))

(defun beanbox--sentinel (process event)
  "Report PROCESS state changes described by EVENT."
  (when (memq (process-status process) '(exit signal))
    (let ((buffer (process-buffer process)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert (format "\n[beanbox] %s" event))))))
    (message "Beanbox %s" (string-trim event))))

(defun beanbox--run (command args &optional server)
  "Run Beanbox COMMAND with ARGS from the configured root.

When SERVER is non-nil, remember the process so `beanbox-stop' can stop it."
  (let* ((default-directory (beanbox--root))
         (buffer (generate-new-buffer (format "*beanbox-%s*" command)))
         (process
          (make-process
           :name (format "beanbox-%s" command)
           :buffer buffer
           :command (append (list (beanbox--program) command) args)
           :connection-type 'pipe
           :noquery t
           :sentinel #'beanbox--sentinel)))
    (with-current-buffer buffer
      (compilation-mode)
      (setq-local default-directory (beanbox--root)))
    (when server
      (when (process-live-p beanbox--server-process)
        (message "已有 Beanbox 服务仍在运行；可用 M-x beanbox-stop 停止它"))
      (setq beanbox--server-process process))
    (display-buffer buffer)
    process))

;;;###autoload
(defun beanbox-start (input)
  "Import INPUT and start the Review UI.

The platform is inferred from INPUT's first directory below `Sources/'."
  (interactive (list (beanbox--read-statement)))
  (let ((platform (beanbox--platform-from-input input)))
    (setq beanbox--last-platform platform)
    (beanbox--run
     "start"
     (list (beanbox--relative input)
           "-c" (beanbox--relative beanbox-config-file)
           "--db" (beanbox--relative (beanbox--database-file platform))
           "-o" (beanbox--relative (beanbox--output-file platform)))
     t)))

;;;###autoload
(defun beanbox-import (input)
  "Import statement INPUT without starting the Review UI."
  (interactive (list (beanbox--read-statement)))
  (let ((platform (beanbox--platform-from-input input)))
    (setq beanbox--last-platform platform)
    (beanbox--run
     "import"
     (list (beanbox--relative input)
           "-c" (beanbox--relative beanbox-config-file)
           "--db" (beanbox--relative (beanbox--database-file platform))))))

;;;###autoload
(defun beanbox-review (platform)
  "Start PLATFORM's Review UI and open a browser."
  (interactive (list (beanbox--read-platform)))
  (setq platform (beanbox--normalize-platform platform)
        beanbox--last-platform platform)
  (beanbox--run
   "review"
   (list "-c" (beanbox--relative beanbox-config-file)
         "--db" (beanbox--relative (beanbox--database-file platform))
         "-o" (beanbox--relative (beanbox--output-file platform))
         "--host" beanbox-review-host
         "--port" (number-to-string beanbox-review-port)
         "--open")
   t))

;;;###autoload
(defun beanbox-export (platform)
  "Export all approved transactions for PLATFORM."
  (interactive (list (beanbox--read-platform)))
  (setq platform (beanbox--normalize-platform platform)
        beanbox--last-platform platform)
  (beanbox--run
   "export"
   (list "-c" (beanbox--relative beanbox-config-file)
         "--db" (beanbox--relative (beanbox--database-file platform))
         "-o" (beanbox--relative (beanbox--output-file platform)))))

;;;###autoload
(defun beanbox-stop ()
  "Stop the most recently started Beanbox Review server."
  (interactive)
  (if (process-live-p beanbox--server-process)
      (progn
        (interrupt-process beanbox--server-process)
        (message "正在停止 Beanbox 服务"))
    (user-error "没有正在运行的 Beanbox 服务")))

;;;###autoload
(defun beanbox ()
  "Choose and run a Beanbox operation."
  (interactive)
  (pcase (completing-read "Beanbox 操作: "
                          '("start" "import" "review" "export" "stop")
                          nil t)
    ("start"  (call-interactively #'beanbox-start))
    ("import" (call-interactively #'beanbox-import))
    ("review" (call-interactively #'beanbox-review))
    ("export" (call-interactively #'beanbox-export))
    ("stop"   (call-interactively #'beanbox-stop))))

(provide 'beanbox)

;;; beanbox.el ends here
