;;; -*- lexical-binding: t -*-

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

(defvar my/idle-loader-forms nil
  "Queue of forms. Each element is a form or (DELAY . FORM).")

(defvar my/idle-loader--start-time nil)
(defvar my/idle-loader--count 0)
(defvar my/idle-loader--errors 0)
(defvar my/idle-loader--timer nil)

(defun my/idle-loader--schedule (delay)
  "Schedule the next queued form after DELAY idle seconds."
  (setq my/idle-loader--timer
        (run-with-idle-timer delay nil #'my/idle-loader-run)))

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

(defun my/idle-loader-run ()
  (if (input-pending-p)
      (my/idle-loader--schedule my/idle-loader-interval)
    (when my/idle-loader-forms
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
                                      my/idle-loader--start-time))))))))

(defun my/idle-loader-start (&optional initial-delay)
  (interactive)
  (when (timerp my/idle-loader--timer)
    (cancel-timer my/idle-loader--timer))
  (setq my/idle-loader--start-time (current-time)
        my/idle-loader--count 0
        my/idle-loader--errors 0)
  (my/idle-loader--log "Starting (%d items)..." (length my/idle-loader-forms))
  (setq my/idle-loader--timer
        (run-with-idle-timer (or initial-delay my/idle-loader-initial-delay)
                             nil #'my/idle-loader-run)))

(defun my/idle-loader-add (&rest forms)
  (setq my/idle-loader-forms (append my/idle-loader-forms forms)))

(add-hook 'window-setup-hook #'my/idle-loader-start)

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
  (add-hook 'server-visit-hook #'on-run-first-buffer-hooks-h))

(add-hook 'window-setup-hook #'on-setup-hooks-h -100)


(provide 'init-util)
