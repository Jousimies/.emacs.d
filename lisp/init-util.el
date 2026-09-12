;;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'seq)

(defmacro add-hook! (hooks &rest rest)
  "A convenience macro for adding N functions to M hooks.

This macro accepts, in order:

  1. The hook(s) to add to.
  2. Optional properties :local, :append, and/or :depth [N].
  3. The function(s) to be added: this can be a quoted function, a quoted list
     thereof, a list of `defun' or `cl-defun' forms, or arbitrary forms (will
     implicitly be wrapped in a lambda).

\(fn HOOKS [:append :local [:depth N] :remove :call-immediately :unless-daemonp-call-immediately] FUNCTIONS-OR-FORMS...)"
  (declare (indent defun))
  (let* ((hook-forms (if (listp hooks) hooks (list hooks)))
         (func-forms ())
         (defn-forms ())
         append-p local-p remove-p call-immediately-p unless-daemonp-call-immediately-p depth)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append-p t))
        (:depth  (setq depth (pop rest)))
        (:local  (setq local-p t))
        (:remove (setq remove-p t))
        (:call-immediately (setq call-immediately-p t))
        (:unless-daemonp-call-immediately
         (setq unless-daemonp-call-immediately-p t))))
    (while rest
      (let* ((next (pop rest))
             (first (car-safe next)))
        (push (cond ((memq first '(function nil)) next)
                    ((eq first 'quote)
                     (let ((quoted (cadr next)))
                       (if (atom quoted)
                           next
                         (when (cdr quoted)
                           (setq rest (cons (list first (cdr quoted)) rest)))
                         (list first (car quoted)))))
                    ((memq first '(defun cl-defun))
                     (push next defn-forms)
                     (list 'function (cadr next)))
                    ((prog1 `(lambda (&rest _) ,@(cons next rest))
                       (setq rest nil))))
              func-forms)))
    `(progn
       ,@defn-forms
       (dolist (func (list ,@func-forms))
         (dolist (hook ',(reverse hook-forms))
           ,(if remove-p
                `(remove-hook hook func ,local-p)
              `(add-hook hook func ,(or depth append-p) ,local-p)))
         ,(cond (call-immediately-p `(funcall func))
                (unless-daemonp-call-immediately-p
                 `(unless (daemonp)
                    (funcall func))))))))

;; Config module loading and diagnostics
(defvar my/config-modules nil
  "Features loaded after the first graphical frame is ready.")

(defvar my/config-module-status nil
  "Alist recording load status and elapsed time for config modules.")

(defvar my/config-modules-loaded-p nil
  "Non-nil after the deferred config module pass has run.")

(defcustom my/config-report-executables '("git" "rg" "bsdtar")
  "External executables shown by `my/config-report'."
  :type '(repeat string)
  :group 'convenience)

(defcustom my/config-report-tree-sit-languages
  '(python javascript typescript json yaml c cpp rust go lua)
  "Tree-sitter grammars checked by `my/config-report'."
  :type '(repeat symbol)
  :group 'convenience)

(defvar my/config-support-modules
  '(init-util init-vars init-font init-modeline init-keys init-funcs
    init-blog init-benchmark)
  "Configuration libraries loaded eagerly, conditionally, or by autoload.")

(defun my/config-cache-state (file)
  "Return a short freshness description for generated cache FILE."
  (cond
   ((not (file-readable-p file)) "MISSING")
   ((seq-some (lambda (source) (file-newer-than-file-p source file))
              (directory-files (expand-file-name "lisp" user-emacs-directory)
                               t "\\`init-.*\\.el\\'"))
    "STALE (config changed)")
   (t "ready")))

(defun my/config-unused-modules ()
  "Return config libraries with no known eager, deferred, or autoload use."
  (let ((known (append my/config-modules my/config-support-modules))
        unused)
    (dolist (file (directory-files
                   (expand-file-name "lisp" user-emacs-directory)
                   t "\\`init-.*\\.el\\'"))
      (let ((feature (intern (file-name-base file))))
        (unless (or (memq feature known)
                    (with-temp-buffer
                      (insert-file-contents file)
                      (search-forward ";;;###autoload" nil t)))
          (push feature unused))))
    (nreverse unused)))

(defun my/config-report-path (label path &optional executable)
  "Print LABEL and status for PATH, optionally requiring EXECUTABLE access."
  (princ (format "%-18s %-8s %s\n"
                 label
                 (cond
                  ((null path) "UNSET")
                  ((and executable (file-executable-p path)) "ready")
                  ((and executable (file-exists-p path)) "NOT-EXEC")
                  ((file-exists-p path) "ready")
                  (t "MISSING"))
                 (or path ""))))

(defun my/require-config-module (feature)
  "Require FEATURE and record its load status and elapsed time.

During a normal startup, report an error and continue with the remaining
modules.  With `--debug-init', preserve the usual fail-fast behavior."
  (let* ((started (current-time))
        (load-module
         (lambda ()
           (require feature)
           (setf (alist-get feature my/config-module-status)
                 (list :state 'loaded
                       :seconds (float-time
                                 (time-subtract nil started))))
           t)))
    (if init-file-debug
        (funcall load-module)
      (condition-case err
          (funcall load-module)
        (error
         (setf (alist-get feature my/config-module-status)
               (list :state 'failed
                     :seconds (float-time (time-subtract nil started))
                     :error err))
         (display-warning 'my/config
                          (format "Could not load %s: %S" feature err)
                          :error)
         nil)))))

(defun my/load-config-modules ()
  "Load each feature in `my/config-modules' exactly once."
  (unless my/config-modules-loaded-p
    (setq my/config-modules-loaded-p t)
    (dolist (feature my/config-modules)
      (my/require-config-module feature))))

(defun my/config-retry-failed-modules ()
  "Retry config modules which failed during the initial load pass."
  (interactive)
  (let ((failed (seq-filter
                 (lambda (feature)
                   (eq (plist-get (alist-get feature my/config-module-status)
                                  :state)
                       'failed))
                 my/config-modules)))
    (if failed
        (progn
          (dolist (feature failed)
            (my/require-config-module feature))
          (my/config-report))
      (message "No failed config modules"))))

(defun my/config-report ()
  "Display startup health, module timings, caches, and external tools."
  (interactive)
  (let ((cache-files '("lisp/load-path-cache.el"
                       "lisp/package-autoloads.el")))
    (with-help-window "*Emacs Configuration Report*"
      (princ (format "Emacs configuration report\n%s\n\n"
                     (make-string 27 ?=)))
      (princ (format "Emacs       %s\nSystem      %s\nConfig      %s\n"
                     emacs-version system-type user-emacs-directory))
      (when after-init-time
        (princ (format "Startup     %.3f seconds\n"
                       (float-time
                        (time-subtract after-init-time before-init-time)))))
      (princ (format "Idle loader %d completed, %d failed, %d queued\n\n"
                     my/idle-loader--count my/idle-loader--errors
                     (length my/idle-loader-forms)))

      (princ "Modules\n-------\n")
      (dolist (feature my/config-modules)
        (let* ((status (alist-get feature my/config-module-status))
               (state (or (plist-get status :state)
                          (and (featurep feature) 'loaded)
                          'pending))
               (seconds (plist-get status :seconds))
               (err (plist-get status :error)))
          (princ (format "%-22s %-7s%s%s\n"
                         feature state
                         (if seconds (format " %7.3fs" seconds) "")
                         (if err (format "  %S" err) "")))))

      (princ "\nGenerated caches\n----------------\n")
      (dolist (relative cache-files)
        (let ((file (expand-file-name relative user-emacs-directory)))
          (princ (format "%-28s %s\n" relative
                         (my/config-cache-state file)))))
      (let ((backup-dir (if (boundp 'my/backup-directory)
                            my/backup-directory
                          (expand-file-name "backups/" cache-directory))))
        (princ (format "%-28s %s\n" ".cache/backups/"
                       (if (file-directory-p backup-dir)
                           "ready"
                         "not created yet"))))

      (princ "\nExternal tools\n--------------\n")
      (dolist (program my/config-report-executables)
        (princ (format "%-12s %s\n"
                       program (or (executable-find program) "MISSING"))))

      (princ "\nFonts\n-----\n")
      (if (not (display-graphic-p))
          (princ "Font discovery requires a graphical frame.\n")
        (dolist (entry `(("Default" . ,(and (boundp 'my/default-font-candidates)
                                             my/default-font-candidates))
                         ("CJK" . ,(and (boundp 'my/cjk-font-candidates)
                                        my/cjk-font-candidates))
                         ("Symbols" . ,(and (boundp 'my/symbol-font-candidates)
                                            my/symbol-font-candidates))))
          (princ (format "%-12s %s\n" (car entry)
                         (or (and (fboundp 'my/first-available-font)
                                  (my/first-available-font (cdr entry)))
                             "MISSING")))))

      (princ "\nTree-sitter grammars\n--------------------\n")
      (dolist (language my/config-report-tree-sit-languages)
        (princ (format "%-12s %s\n"
                       language
                       (if (and (fboundp 'treesit-language-available-p)
                                (ignore-errors
                                  (treesit-language-available-p language)))
                           "ready"
                         "MISSING"))))

      (princ "\nJinx\n----\n")
      (princ (format "%-18s %s\n" "Library"
                     (or (locate-library "jinx") "MISSING")))
      (princ (format "%-18s %s\n" "Native module/source"
                     (or (locate-library "jinx-mod")
                         (locate-library "jinx-mod.c")
                         "MISSING")))
      (princ (format "%-18s %s\n" "Dictionary setting"
                     (if (boundp 'jinx-languages) jinx-languages "en_US (planned)")))

      (princ "\nData paths\n----------\n")
      (my/config-report-path
       "GTD" (and (boundp 'my/org-gtd-directory) my/org-gtd-directory))
      (my/config-report-path
       "Denote" (and (boundp 'my/denote-directory) my/denote-directory))
      (when (boundp 'my/reference-lists)
        (cl-loop for file in my/reference-lists
                 for index from 1
                 do (my/config-report-path (format "BibTeX %d" index) file)))
      (my/config-report-path
       "PDF Tools server"
       (if (boundp 'pdf-info-epdfinfo-program)
           pdf-info-epdfinfo-program
         (expand-file-name "packages/pdf-tools/server/epdfinfo"
                           user-emacs-directory))
       t)

      (princ "\nUnused config modules\n---------------------\n")
      (if-let* ((unused (my/config-unused-modules)))
          (dolist (feature unused) (princ (format "%s\n" feature)))
        (princ "none\n"))
      (princ "\nUse M-x my/config-retry-failed-modules after fixing a failed module.\n"))))

;; idle
(defgroup my-idle-loader nil
  "Ordered automatic idle loading."
  :group 'convenience)

(defcustom my/idle-loader-interval 1
  "Default idle interval between tasks (seconds)."
  :type 'number)

(defcustom my/idle-loader-initial-delay 2
  "Initial delay before starting the idle loader."
  :type 'number)

(defcustom my/idle-loader-log init-file-debug
  "Log each loaded item."
  :type 'boolean)

(defcustom my/idle-loader-log-buffer "*idle-loader*"
  "Log buffer name."
  :type 'string)

(defcustom my/idle-loader-feature-roots
  '(org
    org-id
    org-agenda
    org-capture
    org-clock
    org-attach
    org-refile
    org-goto
    org-archive
    org-edna
    org-gtd)
  "Root features whose dependency order is generated for idle loading."
  :type '(repeat symbol))

(defcustom my/idle-loader-feature-interval 0.5
  "Seconds of idle time between generated feature-loading tasks."
  :type 'number)

(defcustom my/idle-loader-feature-cache-file
  (expand-file-name ".cache/idle-features.el" user-emacs-directory)
  "Generated platform-local feature plan used by the idle loader."
  :type 'file)

(defvar my/idle-loader-forms nil
  "Queue of forms. Each element is a form or (DELAY . FORM).")

(defvar my/idle-loader--start-time nil)
(defvar my/idle-loader--count 0)
(defvar my/idle-loader--errors 0)
(defvar my/idle-loader--timer nil)

;; Set by `my/idle-loader-feature-cache-file'.  Keep these variables defined so
;; a missing or stale generated cache has a predictable fallback path.
(defvar my/idle-loader-generated-system-type nil)
(defvar my/idle-loader-generated-emacs-version nil)
(defvar my/idle-loader-generated-roots nil)
(defvar my/idle-loader-generated-features nil)

(defun my/idle-loader--schedule (delay)
  "Check for the next queued form after DELAY real seconds.

The callback separately verifies that Emacs has also been continuously idle
for DELAY seconds.  A normal timer is intentional here: creating another idle
timer from an idle-timer callback can make it fire immediately when the
current idle period is already longer than DELAY."
  (setq my/idle-loader--timer
        (run-with-timer delay nil #'my/idle-loader-run delay)))

(defun my/idle-loader--ready-p (required-idle)
  "Return non-nil when Emacs is ready to run an idle task.
REQUIRED-IDLE is the minimum continuous idle time in seconds."
  (and (not (input-pending-p))
       (when-let* ((idle-time (current-idle-time)))
         (>= (float-time idle-time) required-idle))))

(defun my/idle-loader--log (fmt &rest args)
  (when my/idle-loader-log
    (let* ((msg (apply #'format fmt args))
           (elapsed (if my/idle-loader--start-time
                        (format "%.2fs"
                                (float-time
                                 (time-subtract (current-time)
                                                my/idle-loader--start-time)))
                      "?")))
      (with-current-buffer (get-buffer-create my/idle-loader-log-buffer)
        (goto-char (point-max))
        (insert (format-time-string "[%H:%M:%S] ")
                (format "[%s] %s\n" elapsed msg)))
      (message "[idle-loader %s] %s" elapsed msg))))

(defun my/idle-loader-run (&optional required-idle)
  "Run one queued task after REQUIRED-IDLE seconds of continuous idleness."
  ;; The one-shot timer which invoked us has expired.
  (setq my/idle-loader--timer nil)
  (let ((required-idle (or required-idle my/idle-loader-interval)))
    (cond
     ((null my/idle-loader-forms))
     ((not (my/idle-loader--ready-p required-idle))
      (my/idle-loader--schedule required-idle))
     (t
        (let* ((item (pop my/idle-loader-forms))
               (delay (if (and (consp item) (numberp (car item)))
                          (car item)
                        my/idle-loader-interval))
               (form  (if (and (consp item) (numberp (car item)))
                          (cdr item)
                        item))
               (desc  (prin1-to-string form))
               (started (current-time)))
          (condition-case err
              (progn
                (eval form t)
                (cl-incf my/idle-loader--count)
                (my/idle-loader--log
                 "OK  %.3fs %s"
                 (float-time (time-subtract nil started)) desc))
            (error
             (cl-incf my/idle-loader--errors)
             (my/idle-loader--log "ERR %s → %S" desc err)))
          (if my/idle-loader-forms
              (my/idle-loader--schedule delay)
            (my/idle-loader--log
             "Finished. Success: %d  Failed: %d  Total: %.2fs"
             my/idle-loader--count
             my/idle-loader--errors
             (float-time (time-subtract (current-time)
                                        my/idle-loader--start-time)))))))))

(defun my/idle-loader-start (&optional initial-delay)
  (interactive)
  ;; This is a one-shot UI hook, especially important for daemon frames.
  (remove-hook 'window-setup-hook #'my/idle-loader-start)
  (remove-hook 'server-after-make-frame-hook #'my/idle-loader-start)
  (when (timerp my/idle-loader--timer)
    (cancel-timer my/idle-loader--timer))
  (setq my/idle-loader--start-time (current-time)
        my/idle-loader--count 0
        my/idle-loader--errors 0)
  (my/idle-loader--log "Starting (%d items)..." (length my/idle-loader-forms))
  (my/idle-loader--schedule (or initial-delay my/idle-loader-initial-delay)))

(defun my/idle-loader-add-features (features &optional delay)
  "Queue FEATURES individually, using DELAY between loads."
  (apply #'my/idle-loader-add
         (mapcar
          (lambda (feature)
            (let ((form `(require ',feature nil t)))
              (if delay
                  (cons delay form)
                form)))
          features)))

(defun my/idle-loader--load-generated-features ()
  "Return a valid generated feature plan, or nil when none is usable."
  (setq my/idle-loader-generated-system-type nil
        my/idle-loader-generated-emacs-version nil
        my/idle-loader-generated-roots nil
        my/idle-loader-generated-features nil)
  (condition-case err
      (when (file-readable-p my/idle-loader-feature-cache-file)
        (load my/idle-loader-feature-cache-file nil t)
        (when (and (eq my/idle-loader-generated-system-type system-type)
                   (equal my/idle-loader-generated-emacs-version emacs-version)
                   (equal my/idle-loader-generated-roots
                          my/idle-loader-feature-roots)
                   (consp my/idle-loader-generated-features)
                   (seq-every-p #'symbolp
                                my/idle-loader-generated-features))
          my/idle-loader-generated-features))
    (error
     (my/idle-loader--log "Ignoring feature cache: %S" err)
     nil)))

(defun my/idle-loader-add-feature-roots ()
  "Queue the generated dependency plan for `my/idle-loader-feature-roots'.
Fall back to loading only the roots when the generated cache is missing,
stale, malformed, or belongs to another Emacs platform/version."
  (my/idle-loader-add-features
   (or (my/idle-loader--load-generated-features)
       my/idle-loader-feature-roots)
   my/idle-loader-feature-interval))

(defun my/idle-loader-add (&rest forms)
  (setq my/idle-loader-forms (append my/idle-loader-forms forms)))

(add-hook (if (daemonp) 'server-after-make-frame-hook 'window-setup-hook)
          #'my/idle-loader-start)

;; Lightweight replacement for on.el.  Loading the external package showed up
;; prominently in `sanityinc/require-times'; these hooks are all we use.
(defvar on-first-input-hook nil
  "Transient hooks run before the first user input.")
(defvar on-first-file-hook nil
  "Transient hooks run before the first interactively opened file.")
(defvar on-first-buffer-hook nil
  "Transient hooks run before the first interactively opened buffer.")
(defvar on-init-ui-hook nil
  "List of hooks to run when the UI has been initialized.")

(defun on-run-first-input-hooks-h (&rest _)
  (run-hooks 'on-first-input-hook)
  (remove-hook 'pre-command-hook #'on-run-first-input-hooks-h))

(defun on-run-first-file-hooks-h (&rest _)
  (run-hooks 'on-first-file-hook)
  (advice-remove 'after-find-file #'on-run-first-file-hooks-h)
  (remove-hook 'dired-initial-position-hook #'on-run-first-file-hooks-h))

(defun on-run-first-buffer-hooks-h (&rest _)
  (run-hooks 'on-first-buffer-hook)
  (advice-remove 'after-find-file #'on-run-first-buffer-hooks-h)
  (remove-hook 'window-buffer-change-functions #'on-run-first-buffer-hooks-h)
  (remove-hook 'server-visit-hook #'on-run-first-buffer-hooks-h))

(defun on-run-init-ui-hooks-h (&rest _)
  (run-hooks 'on-init-ui-hook)
  (remove-hook 'server-after-make-frame-hook #'on-run-init-ui-hooks-h)
  (remove-hook 'after-init-hook #'on-run-init-ui-hooks-h))

(add-hook (if (daemonp) 'server-after-make-frame-hook 'after-init-hook)
          #'on-run-init-ui-hooks-h)

(defun on-setup-hooks-h ()
  (add-hook 'pre-command-hook #'on-run-first-input-hooks-h)
  (advice-add 'after-find-file :before #'on-run-first-file-hooks-h)
  (add-hook 'dired-initial-position-hook #'on-run-first-file-hooks-h)
  (advice-add 'after-find-file :before #'on-run-first-buffer-hooks-h)
  (add-hook 'window-buffer-change-functions #'on-run-first-buffer-hooks-h)
  (add-hook 'server-visit-hook #'on-run-first-buffer-hooks-h)
  (remove-hook 'window-setup-hook #'on-setup-hooks-h)
  (remove-hook 'server-after-make-frame-hook #'on-setup-hooks-h))

(add-hook (if (daemonp) 'server-after-make-frame-hook 'window-setup-hook)
          #'on-setup-hooks-h -100)


(provide 'init-util)
