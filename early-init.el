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

;; https://www.emacswiki.org/emacs/ExecPath
(defun set-exec-path-from-shell-PATH ()
  "This is particularly useful under Mac OS X and macOS."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$" "" (shell-command-to-string
                                          "$SHELL --login -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when (eq system-type 'darwin)
  (set-exec-path-from-shell-PATH))

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(scroll-bar-width . 5) default-frame-alist)
(push '(undecorated . t) default-frame-alist)
(push '(width . 100) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(fullscreen . fullscreen) initial-frame-alist)

(define-fringe-bitmap 'right-curly-arrow  [])
(define-fringe-bitmap 'left-curly-arrow  [])


(setq ns-use-native-fullscreen nil)
(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'modus-operandi-tritanopia t))
    ('dark (load-theme 'modus-vivendi-tinted t))))
(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

(setq byte-compile-warnings nil)

;; Hide information
;; For information about GNU Emacs and the GNU system, type <f1> C-a.
(fset 'display-startup-echo-area-message 'ignore)

;; use package.el install packages
(setq package-enable-at-startup nil)
(setq use-package-enable-imenu-support t)

;;; early-init.el ends here
