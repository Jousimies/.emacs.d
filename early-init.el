;;; early-init.el --- Early Init File -*- lexical-binding: t; no-byte-compile: t -*-
;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)
;; Do not allow loading from the package cache (same reason).
(setq package-quickstart nil)
(setq package--init-file-ensured t)
;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(fullscreen . maximized) default-frame-alist)
;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)
(setq inhibit-compacting-font-caches t)
(setq use-file-dialog nil)
(setq use-dialog-box nil)
;; Make the initial buffer load faster by setting its mode to fundamental-mode
;; (setq initial-major-mode 'fundamental-mode)
;; Prevent unwanted runtime builds in gccemacs (native-comp); packages are
;; compiled ahead-of-time when they are installed and site files are compiled
;; when gccemacs is installed.
(setq comp-deferred-compilation nil)
;; Disable mode-line, It's uglily after theme changed
;; (setq-default mode-line-format nil)


;; https://github.com/Ergus/mini_dotemacs
(defsubst my/unset-gc ()
  "Disable the gc."
  (setq gc-cons-threshold most-positive-fixnum   ;; Defer Garbage collection
	    gc-cons-percentage 1.0))

(defsubst my/restore-gc ()
  "Restore the gc."
  (setq gc-cons-threshold my/gc-cons-threshold
	    gc-cons-percentage 0.1))

;;; early-init.el ends here
