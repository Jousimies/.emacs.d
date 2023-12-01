;;; early-init.el --- Early Init File -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

;; Profiling since here when in debug-mode
(when init-file-debug
  (profiler-start 'cpu)
  (add-hook 'window-setup-hook #'profiler-stop 0))

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0)

;; https://github.com/seagle0128/.emacs.d/blob/master/init.el
(setq auto-mode-case-fold nil)

(unless (or (daemonp) noninteractive init-file-debug)
  ;; Suppress file handlers operations at startup
  ;; `file-name-handler-alist' is consulted on each call to `require' and `load'
  (let ((old-value file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist old-value))))
              101)))

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

(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))
(setq ns-use-native-fullscreen nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . 0) default-frame-alist)
(push '(undecorated . t) default-frame-alist)
(push '(width . 100) default-frame-alist)
(push '(fullscreen . fullscreen) initial-frame-alist)

(setq byte-compile-warnings nil)

(fset 'display-startup-echo-area-message 'ignore)

(add-to-list 'load-path "~/.emacs.d/packages/ef-themes/")
(require 'ef-themes)
;; modus-themes 会导致 modeline 的字符计算不准确，因而右侧会超出屏幕范围。
;; ef-themes 没有上述问题。
(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'ef-maris-light t))
    ('dark (load-theme 'ef-maris-dark t))))

(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

(global-unset-key (kbd "s-l"))
(global-unset-key (kbd "s-n"))

;;; early-init.el ends here
