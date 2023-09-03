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

;; Disable mode-line when emacs startup.
(setq-default mode-line-format nil)

(setq byte-compile-warnings nil)

(fset 'display-startup-echo-area-message 'ignore)
;; (load-theme 'modus-vivendi t)
(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'modus-operandi t))
    ('dark (load-theme 'modus-vivendi t))))

(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

;;; early-init.el ends here
