;; init.el --- Personal Emacs Configuration -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

;; https://github.com/seagle0128/.emacs.d/blob/master/init.el
(setq auto-mode-case-fold nil)

(unless (or (daemonp) noninteractive init-file-debug)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist
                                           old-file-name-handler-alist)))))))

;; https://github.com/manateelazycat/lazycat-emacs/blob/master/site-start.el
(require 'cl-lib)
(defun add-subdirs-to-load-path (search-dir)
  (interactive)
  (let* ((dir (file-name-as-directory search-dir)))
    (dolist (subdir
             ;; 过滤出不必要的目录，提升Emacs启动速度
             (cl-remove-if
              #'(lambda (subdir)
                  (or
                   ;; 不是目录的文件都移除
                   (not (file-directory-p (concat dir subdir)))
                   ;; 父目录、 语言相关和版本控制目录都移除
                   (member subdir '("." ".."
                                    "dist" "node_modules" "__pycache__"
                                    "RCS" "CVS" "rcs" "cvs" ".git" ".github"))))
              (directory-files dir)))
      (let ((subdir-path (concat dir (file-name-as-directory subdir))))
        ;; 目录下有 .el .so .dll 文件的路径才添加到 `load-path' 中，提升Emacs启动速度
        (when (cl-some #'(lambda (subdir-file)
                           (and (file-regular-p (concat subdir-path subdir-file))
                                ;; .so .dll 文件指非Elisp语言编写的Emacs动态库
                                (member (file-name-extension subdir-file) '("el" "so" "dll"))))
                       (directory-files subdir-path))

          ;; 注意：`add-to-list' 函数的第三个参数必须为 t ，表示加到列表末尾
          ;; 这样Emacs会从父目录到子目录的顺序搜索Elisp插件，顺序反过来会导致Emacs无法正常启动
          (add-to-list 'load-path subdir-path t))

        ;; 继续递归搜索子目录
        (add-subdirs-to-load-path subdir-path)))))

;; add packages to load-path
(add-subdirs-to-load-path "~/.emacs.d/packages")

;; add lisp configuraton
(add-to-list 'load-path "~/.emacs.d/lisp")

;; https://www.emacswiki.org/emacs/ExecPath
(defun set-exec-path-from-shell-PATH ()
  "This is particularly useful under Mac OS X and macOS."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$" "" (shell-command-to-string
                                          "$SHELL --login -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-PATH)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(defvar my-cloud "~/Nextcloud"
  "This folder is My cloud.")

;; L.Personal.Galaxy location may change, but folders in this directory never change.
(defvar my-galaxy (expand-file-name "L.Personal.Galaxy" my-cloud)
  "This folder stores all the plain text files of my life.")

(defvar website-directory "~/Nextcloud/L.Personal.Galaxy/website"
  "The source folder of my blog.")

(setq ring-bell-function 'ignore)
(setq tab-width 4)
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq use-short-answers t)
(setq read-process-output-max #x10000)
(setq create-lockfiles nil)
(setq recenter-redisplay nil)
(setq load-prefer-newer t)
(setq next-screen-context-lines 5)
(setq frame-inhibit-implied-resize t)
(setq inhibit-compacting-font-caches t)
(setq inhibit-quit nil)
(setq fast-but-imprecise-scrolling t)
(setq scroll-preserve-screen-position t)
(setq auto-save-list-file-name nil)
(setq history-delete-duplicates t)
(setq bidi-display-reordering nil)
(setq read-buffer-completion-ignore-case t)
(setq completion-ignore-case t)
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(setq max-mini-window-height 10)

;; (setq use-package-compute-statistics t)
;; (setq use-package-verbose t)
;; (require 'init-benchmark)
(require 'init-recentf)
(require 'init-dashboard)
(add-hook 'after-init-hook #'(lambda ()
                               (require 'init-font)
                               (require 'init-theme)
                               (require 'init-tab)
                               (require 'init-frame)
                               (require 'init-evil)
                               (require 'init-ui)
                               (require 'init-builtin)
                               (require 'init-window)))

(run-with-timer 1 nil  #'(lambda ()
                           (require 'init-evil+)
                           (require 'init-buffer)
                           (require 'init-dired)
                           (require 'init-edit)
                           (require 'init-completion)
                           (require 'init-template)
                           (require 'init-prog)
                           (require 'init-rime)
                           (require 'init-misc)
                           (require 'init-search)
                           (require 'init-git)
                           (require 'init-helpful)
                           (require 'init-spell)
                           (require 'init-latex)
                           (require 'init-translate)
                           (require 'init-org)
                           (require 'init-org+)
                           (require 'init-note)
                           (require 'init-blog)
                           (require 'init-bib)
                           (require 'init-gtd)
                           (require 'init-finance)
                           (require 'init-reader)
                           (require 'init-elfeed)
                           (require 'init-mail)
                           (require 'init-telega)
                           (require 'init-vterm)
                           (require 'init-chatgpt)))

(use-package server
  :hook (after-init . server-start)
  :bind ("C-c q r" . restart-emacs))

(use-package gcmh
  :hook ((after-init . gcmh-mode)
         (focus-out . garbage-collect))
  :config
  (setq gcmh-idle-delay 'auto)
  (setq gcmh-auto-idle-delay-factor 10)
  (setq gcmh-high-cons-threshold #x1000000))

(add-hook 'window-setup-hook
          (lambda ()
            (garbage-collect)
            (let ((curtime (current-time)))
              (message "Times: init:%.06f total:%.06f gc-done:%d"
                       (float-time (time-subtract after-init-time before-init-time))
                       (float-time (time-subtract curtime before-init-time))
                       gcs-done)))
          90)

(provide 'init)
;;; init.el ends here.
