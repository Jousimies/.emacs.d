;; -*- lexical-binding: t; -*-

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0)
(setq package-enable-at-startup nil)

;; Increase process read size before any package can start subprocesses.
(setq read-process-output-max (* 4 1024 1024))

(setq load-prefer-newer t)

;; Emacs startup performance
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

(setq load-path-filter-function #'load-path-filter-cache-directory-files)

;; Performance on Windows is considerably worse than elsewhere. We'll need
;; everything we can get.
(when (boundp 'w32-get-true-file-attributes)
  (setq w32-get-true-file-attributes nil    ; reduce IO ops
        w32-pipe-read-delay 0               ; faster IPC
        w32-pipe-buffer-size (* 64 1024)))  ; read more at a time (was 4K)

;; https://www.emacswiki.org/emacs/ExecPath
(when (eq system-type 'darwin)
  (defun set-exec-path-from-shell-PATH ()
    "This is particularly useful under Mac OS X and macOS."
    (interactive)
    (let ((path-from-shell (replace-regexp-in-string
                            "[ \t\n]*$" "" (shell-command-to-string
                                            "$SHELL --login -c 'echo $PATH'"))))
      (setenv "PATH" path-from-shell)
      (setq exec-path (split-string path-from-shell path-separator))))

  (set-exec-path-from-shell-PATH))

(put 'if-let 'byte-obsolete-info nil)
(put 'when-let 'byte-obsolete-info nil)
(setq warning-suppress-log-types '((files)))

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Inhibit startup screen & message
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t
      inhibit-startup-buffer-menu t
      inhibit-x-resources t
      inhibit-default-init t
      initial-major-mode 'fundamental-mode)
(advice-add #'display-startup-echo-area-message :override #'ignore)
(advice-add #'display-startup-screen :override #'ignore)

;; Suppress flashing at startup
(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (unless (daemonp)
              (redraw-frame))))

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(scroll-bar-width . 5) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(left-fringe . 0) default-frame-alist)
(push '(right-fringe . 0) default-frame-alist)

(when (featurep 'ns)
  (push '(undecorated . t) default-frame-alist)
  (push '(fullscreen . maximized) initial-frame-alist)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

(when (eq system-type 'windows-nt)
  (push '(width . 150) default-frame-alist)
  (push '(height . 50) default-frame-alist))

;; (blink-cursor-mode -1)

(if (> emacs-major-version 31)
    (setq-default mode-line-invisible-mode t)
  (setq-default mode-line-format nil))

(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'doric-light t))
    ('dark (load-theme 'modus-vivendi-deuteranopia t))))
(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

(cond ((eq system-type 'windows-nt)
       (when initial-window-system
	 (load-theme 'modus-operandi-tritanopia t)))
      ((featurep 'ns)
       (defun my/apply-theme (appearance)
	 "Load theme, taking current system APPEARANCE into consideration."
	 (mapc #'disable-theme custom-enabled-themes)
	 (pcase appearance
	   ('light (load-theme 'modus-operandi-tritanopia t))
	   ('dark (load-theme 'modus-vivendi t))))
       (add-hook 'ns-system-appearance-change-functions #'my/apply-theme)
       (when (boundp 'ns-system-appearance)
	 (my/apply-theme ns-system-appearance))))
