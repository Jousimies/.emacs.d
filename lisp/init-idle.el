;; -*- lexical-binding: t; -*-

;;; use-package :idle 关键字 + 自动 idle 加载

(require 'use-package-core)

(defgroup my-idle-loader nil
  "Ordered automatic idle loading."
  :group 'convenience)

(defcustom my/idle-loader-interval 1
  "Default idle interval between tasks (seconds)."
  :type 'number)

(defcustom my/idle-loader-initial-delay 2
  "Initial delay before starting the idle loader."
  :type 'number)

(defcustom my/idle-loader-log t
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
      (run-with-idle-timer my/idle-loader-interval nil #'my/idle-loader-run)
    (when my/idle-loader-forms
      (let* ((item (pop my/idle-loader-forms))
             (delay (if (and (consp item) (numberp (car item)))
                        (car item)
                      my/idle-loader-interval))
             (form  (if (and (consp item) (numberp (car item)))
                        (cdr item)
                      item))
             (desc  (prin1-to-string form)))
        (condition-case err
            (progn
              (eval form t)
              (cl-incf my/idle-loader--count)
              (my/idle-loader--log "OK  %s" desc))
          (error
           (cl-incf my/idle-loader--errors)
           (my/idle-loader--log "ERR %s → %S" desc err)))
        (if my/idle-loader-forms
            (run-with-idle-timer delay nil #'my/idle-loader-run)
          (my/idle-loader--log
           "Finished. Success: %d  Failed: %d  Total: %.2fs"
           my/idle-loader--count
           my/idle-loader--errors
           (float-time (time-subtract (current-time)
                                      my/idle-loader--start-time))))))))

(defun my/idle-loader-start (&optional initial-delay)
  (interactive)
  (setq my/idle-loader--start-time (current-time)
        my/idle-loader--count 0
        my/idle-loader--errors 0)
  (my/idle-loader--log "Starting (%d items)..." (length my/idle-loader-forms))
  (run-with-idle-timer (or initial-delay my/idle-loader-initial-delay)
                       nil #'my/idle-loader-run))

(defun my/idle-loader-add (&rest forms)
  (setq my/idle-loader-forms (append my/idle-loader-forms forms)))


;;; use-package keyword: :idle

(push :idle use-package-keywords)

(defun use-package-normalize/:idle (_name keyword args)
  "Normalize :idle keyword.
ARGS can be:
  t              → (require 'NAME nil t)
  number         → (number . (require 'NAME nil t))
  form           → form
  (number form)  → (number . form)"
  (use-package-only-one (symbol-name keyword) args
    (lambda (_label arg)
      (cond
       ((eq arg t) t)
       ((numberp arg) arg)
       ((and (consp arg) (numberp (car arg))) arg)
       (t arg)))))

(defun use-package-handler/:idle (name _keyword arg rest state)
  "Handle :idle keyword. Add to idle loader queue."
  (let* ((body (use-package-process-keywords name rest state))
         (form (cond
                ((eq arg t)
                 `(require ',name nil t))
                ((numberp arg)
                 `(,arg . (require ',name nil t)))
                ((and (consp arg) (numberp (car arg)))
                 arg)
                (t arg))))
    `((my/idle-loader-add ',form)
      ,@body)))


;;; 启动

(add-hook 'after-init-hook #'my/idle-loader-start)

(require 'on)

(use-package gcmh
  :hook (on-first-buffer . gcmh-mode)
  :config
  (setq gc-cons-percentage 0.1)
  (setq gcmh-idle-delay 'auto)
  (setq gcmh-auto-idle-delay-factor 10)
  (setq gcmh-high-cons-threshold #x1000000))

(advice-add 'after-focus-change-function :after 'garbage-collect)

(setq my/idle-loader-log nil)
(setq use-package-expand-minimally t)
(setq use-package-verbose t)
(setq use-package-compute-statistics t)
(setq use-package-minimum-reported-time 0)

;; Server
(use-package server
  :idle t
  :config
  (unless (server-running-p)
    (server-start)))

(provide 'init-idle)
