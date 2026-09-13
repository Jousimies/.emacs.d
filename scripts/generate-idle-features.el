;;; generate-idle-features.el --- Discover idle-load dependencies  -*- lexical-binding: t; -*-

;; This helper is invoked by update_emacs.py.  It loads the normal
;; configuration in a fresh batch Emacs, replays every queued idle task, and
;; emits a task-aware dependency plan as machine-readable lines.

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

(defun my/idle-generate--task-form (item)
  "Return the executable form stored in idle queue ITEM."
  (if (and (consp item) (numberp (car item)))
      (cdr item)
    item))

(defun my/idle-generate--require-form-p (form)
  "Return non-nil when FORM is a plain feature `require'."
  (and (consp form) (eq (car form) 'require)))

(defun my/idle-generate--expand-task (item)
  "Replay idle ITEM and return its dependency-aware task sequence."
  (let ((before (make-hash-table :test #'eq))
        (form (my/idle-generate--task-form item))
        (succeeded t))
    (mapc (lambda (feature) (puthash feature t before)) features)
    (condition-case err
        (let ((result (eval form t)))
          (when (and (my/idle-generate--require-form-p form)
                     (null result))
            (error "Required feature was not found: %S" form)))
      (error
       (setq succeeded nil)
       ;; Keep opaque/UI-specific tasks in the runtime plan even when batch
       ;; discovery cannot execute them.  Dependencies loaded before the error
       ;; are still useful and are captured below.
       (princ (format "IDLE_WARNING\t%S -> %S\n" form err))))
    (append
     (mapcar
      (lambda (feature)
        (cons my/idle-loader-feature-interval
              `(require ',feature nil t)))
      (my/idle-generate--new-features before))
     ;; Only a successful require is fully represented by its feature chain.
     ;; Other and opaque tasks must still run at runtime.
     (unless (and succeeded (my/idle-generate--require-form-p form))
       (list item)))))

(let ((user-emacs-directory my/idle-generate--config-directory)
      (init-file-debug nil)
      (inhibit-message t))
  (load (expand-file-name "early-init.el" user-emacs-directory) nil t)
  (load (expand-file-name "init.el" user-emacs-directory) nil t)
  ;; Batch mode has no graphical window setup, so run the same deferred module
  ;; pass explicitly.  Modules enqueue the raw roots and arbitrary idle forms;
  ;; the generated cache is only consumed when the idle loader starts.
  (my/load-config-modules)
  (when (timerp my/idle-loader--timer)
    (cancel-timer my/idle-loader--timer))
  (setq my/idle-loader--timer nil)
  (let* ((raw-forms (copy-tree my/idle-loader-forms))
         (source-signature (my/idle-loader-source-signature raw-forms))
         (plan (apply #'append
                      (mapcar #'my/idle-generate--expand-task raw-forms))))
    (princ (format "IDLE_META\tsystem-type\t%s\n" system-type))
    (princ (format "IDLE_META\temacs-version\t%s\n" emacs-version))
    (princ (format "IDLE_META\tsource-signature\t%s\n" source-signature))
    (dolist (item plan)
      (princ "IDLE_TASK\t")
      (prin1 item)
      (terpri))))

;;; generate-idle-features.el ends here
