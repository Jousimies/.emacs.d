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

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . 0) default-frame-alist)
(push '(undecorated . t) default-frame-alist)
(push '(width . 100) default-frame-alist)
(push '(fullscreen . maximized) initial-frame-alist)

;; To suppress flashing at startup
(setq-default inhibit-redisplay t
              inhibit-message t)

(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

;; Disable mode-line when emacs startup.
(setq-default mode-line-format nil)

(setq byte-compile-warnings nil)

(fset 'display-startup-echo-area-message 'ignore)

;;; early-init.el ends here
