;;; generate-idle-features.el --- Discover idle-load dependencies  -*- lexical-binding: t; -*-

;; This helper is invoked by update_emacs.py.  It loads the normal
;; configuration in a fresh batch Emacs, requires the configured root
;; features, and emits the observed feature order as machine-readable lines.

(require 'cl-lib)

(defconst my/idle-generate--script-file
  (or load-file-name buffer-file-name)
  "Absolute path of this generator script.")

(defconst my/idle-generate--config-directory
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory my/idle-generate--script-file)))
  "Configuration directory inferred from this script's location.")

(defun my/idle-generate--new-features (before)
  "Return loadable features provided after BEFORE was populated."
  (let (result)
    ;; `features' stores newest entries first; reverse a copy to recover the
    ;; actual provision order without mutating the global list.
    (dolist (feature (nreverse (copy-sequence features)))
      (when (and (not (gethash feature before))
                 ;; Some libraries provide aliases with no same-named file,
                 ;; e.g. bytecomp.el provides `byte-compile'.  Loading the
                 ;; real provider elsewhere in the plan will provide them.
                 (locate-library (symbol-name feature)))
        (push feature result)))
    (nreverse result)))

(let ((user-emacs-directory my/idle-generate--config-directory)
      (init-file-debug nil)
      (inhibit-message t))
  (load (expand-file-name "early-init.el" user-emacs-directory) nil t)
  (load (expand-file-name "init.el" user-emacs-directory) nil t)
  ;; Batch mode has no graphical window setup, so run the same deferred module
  ;; pass explicitly.  The modules only queue idle forms; they do not consume
  ;; the generated plan in this process.
  (my/load-config-modules)
  (when (timerp my/idle-loader--timer)
    (cancel-timer my/idle-loader--timer))
  (setq my/idle-loader--timer nil
        my/idle-loader-forms nil)
  (let ((before (make-hash-table :test #'eq)))
    (mapc (lambda (feature) (puthash feature t before)) features)
    (dolist (root my/idle-loader-feature-roots)
      (unless (require root nil t)
        (error "Unable to load idle feature root: %S" root)))
    (princ (format "IDLE_META\tsystem-type\t%s\n" system-type))
    (princ (format "IDLE_META\temacs-version\t%s\n" emacs-version))
    (dolist (root my/idle-loader-feature-roots)
      (princ (format "IDLE_ROOT\t%s\n" root)))
    (dolist (feature (my/idle-generate--new-features before))
      (princ (format "IDLE_FEATURE\t%s\n" feature)))))

;;; generate-idle-features.el ends here
