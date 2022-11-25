;;; init.el --- Emacs configuration.   -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:

;; emacs-plus auto inject PATH value into Emacs.app/Contents/Info.plist.
;; Otherwise, use below function to do the job.
;; https://www.emacswiki.org/emacs/ExecPath
;; (defun set-exec-path-from-shell-PATH ()
;;   "This is particularly useful under Mac OS X and macOS."
;;   (interactive)
;;   (let ((path-from-shell (replace-regexp-in-string
;;                           "[ \t\n]*$" "" (shell-command-to-string
;;                                           "$SHELL --login -c 'echo $PATH'"))))

;;     (setenv "PATH" path-from-shell)
;;     (setq exec-path (split-string path-from-shell path-separator))))

;; (set-exec-path-from-shell-PATH)

;; Enable tray or modeline
;; (require 'init-tray)

;; Evil, Meow or origin.
;; (require 'init-meow)
;; (require 'init-key)

;; Message startup time if init-dashboard not activated.
;; (let ((package-count 0) (time (emacs-init-time)))
;;   (when (bound-and-true-p package-alist)
;;     (setq package-count (length package-activated-list)))
;;   (if (zerop package-count)
;;       (message "Emacs started in %s" time)
;;     (message "%d packages loaded in %s" package-count time)))

;;; Code:


;; Speedup Emacs
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

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)


;; Load path
;; https://github.com/seagle0128/.emacs.d/blob/master/init.el
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'.
Don't put large files in `site-lisp' directory, e.g. EAF.
Otherwise the startup will be very slow."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)


;; package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))


;; https://github.com/purcell/emacs.d/blob/master/lisp/init-elpa.el
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.)
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (when (stringp min-version)
    (setq min-version (version-to-list min-version)))
  (or (package-installed-p package min-version)
      (let* ((known (cdr (assoc package package-archive-contents)))
             (best (car (sort known (lambda (a b)
                                      (version-list-<= (package-desc-version b)
                                                       (package-desc-version a)))))))
        (if (and best (version-list-<= min-version (package-desc-version best)))
            (package-install best)
          (if no-refresh
              (error "No version of %s >= %S is available" package min-version)
            (package-refresh-contents)
            (require-package package min-version t)))
        (package-installed-p package min-version))))

(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.)
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))

;; Persoanl Info
(setq user-full-name "Duan Ning")
(setq user-mail-address "duan_n@outlook.com")

;; Startup test.
;; (profiler-start 'cpu)
;; (toggle-debug-on-error)

;; Benchmark
;; https://github.com/purcell/emacs.d/blob/master/lisp/init-benchmarking.el
(require 'init-benchmarking)

(require 'init-autoload)
(require 'init-variables)
(require 'on)

(require 'init-ui)
(require 'init-dashboard)
(require 'init-modeline)

(require 'init-evil)

(require 'init-general)
(require 'init-builtin)
(require 'init-recentf)

(require 'init-utils)
(require 'init-git)
(require 'init-dired)
(require 'init-bookmark)
(require 'init-proxy)

(require 'init-minibuffer)
(require 'init-completion)
;; (require 'init-persp)

(require 'init-rime)
(require 'init-browser)
(require 'init-rg)
(require 'init-tempel)

(require 'init-spell)
(require 'init-dict)

(require 'init-org)
(require 'init-org+)
(require 'init-roam)
(require 'init-denote)
(require 'init-finance)
(require 'init-bibtex)
(require 'init-blog)
(require 'init-latex)

(require 'init-flymake)
(require 'init-markdown)
(require 'init-python)

(require 'init-rss)
(require 'init-reader)
(require 'init-calibre)
(require 'init-shell)
(require 'init-mail)
(require 'init-telega)
(require 'init-eaf)

(require 'init-osm)
(require 'init-fun)

;; Custom file
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))


;; Gcmh
(when (maybe-require-package 'gcmh)
  (gcmh-mode)
  (setq gcmh-idle-delay 'auto)
  (setq gcmh-auto-idle-delay-factor 10)
  (setq gcmh-high-cons-threshold #x1000000))


(provide 'init)
;;; init.el ends here.
