;;; profile-idle-loader.el --- Profile the configured idle queue  -*- lexical-binding: t; -*-

;; Run from a terminal with:
;;
;;   emacs --batch -Q -l ~/.emacs.d/scripts/profile-idle-loader.el
;;
;; On Windows (cmd.exe):
;;
;;   emacs.exe --batch -Q -l "%USERPROFILE%\.emacs.d\scripts\profile-idle-loader.el"

(require 'cl-lib)
(require 'seq)

(defconst my/idle-profile--script-file
  (or load-file-name buffer-file-name)
  "Absolute path of this profiling script.")

(defconst my/idle-profile--config-directory
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory my/idle-profile--script-file)))
  "Configuration directory inferred from this script's location.")

(defun my/idle-profile--item-parts (item)
  "Return (DELAY FORM) for an idle queue ITEM."
  (if (and (consp item) (numberp (car item)))
      (list (car item) (cdr item))
    (list my/idle-loader-interval item)))

(defun my/idle-profile--new-features (before)
  "Return features loaded since the BEFORE hash table was populated."
  (seq-remove (lambda (feature) (gethash feature before))
              (nreverse (copy-sequence features))))

(defun my/idle-profile--print-module-times ()
  "Print configuration module load status and elapsed time."
  (princ "\nConfiguration modules\n")
  (princ "---------------------\n")
  (dolist (feature my/config-modules)
    (let* ((status (alist-get feature my/config-module-status))
           (state (or (plist-get status :state) 'pending))
           (seconds (or (plist-get status :seconds) 0.0))
           (error-data (plist-get status :error)))
      (princ (format "%-22s %-7s %8.3fs%s\n"
                     feature state seconds
                     (if error-data (format "  %S" error-data) ""))))))

(defun my/idle-profile--print-queue (queue)
  "Print QUEUE without evaluating it."
  (princ (format "\nIdle queue (%d items)\n" (length queue)))
  (princ "---------------------\n")
  (cl-loop for item in queue
           for index from 1
           for (delay form) = (my/idle-profile--item-parts item)
           do (princ (format "%2d. delay-after=%5s  %S\n"
                             index
                             (if (< index (length queue))
                                 (format "%.2fs" delay)
                               "-")
                             form))))

(defun my/idle-profile--run-queue (queue)
  "Evaluate a copy of QUEUE immediately and print per-item timings."
  (let ((total-started (current-time))
        (successes 0)
        (failures 0)
        (nominal-delays 0.0)
        (total (length queue)))
    (princ "\nExecution timings (configured idle waits are skipped)\n")
    (princ "------------------------------------------------------\n")
    (cl-loop for item in queue
             for index from 1
             for (delay form) = (my/idle-profile--item-parts item)
             do
             (let ((before (make-hash-table :test #'eq))
                   (before-gcs gcs-done)
                   (started (current-time))
                   state error-data elapsed newly-loaded)
               (mapc (lambda (feature) (puthash feature t before)) features)
               (princ (format "RUN %2d/%d  %S\n" index total form))
               (condition-case err
                   (progn
                     (eval form t)
                     (setq state "OK")
                     (cl-incf successes))
                 (error
                  (setq state "ERR"
                        error-data err)
                  (cl-incf failures)))
               (setq elapsed (float-time (time-subtract nil started))
                     newly-loaded (my/idle-profile--new-features before))
               ;; The loader uses an item's delay before scheduling the next
               ;; item; the final item's delay is therefore not consumed.
               (when (< index total)
                 (cl-incf nominal-delays delay))
               (princ (format "    %-3s %8.3fs  features +%-3d  GC +%d%s\n"
                              state elapsed (length newly-loaded)
                              (- gcs-done before-gcs)
                              (if error-data (format "  %S" error-data) "")))
               (when newly-loaded
                 (princ (format "        loaded: %s\n"
                                (mapconcat #'symbol-name newly-loaded ", "))))))
    (let ((execution-time (float-time (time-subtract nil total-started))))
      (princ "\nSummary\n")
      (princ "-------\n")
      (princ (format "Success / failed       %d / %d\n" successes failures))
      (princ (format "Queue execution time   %.3fs\n" execution-time))
      (princ (format "Initial idle delay     %.3fs\n"
                     my/idle-loader-initial-delay))
      (princ (format "Between-item delays    %.3fs\n" nominal-delays))
      (princ (format "Nominal wall time      %.3fs (execution + configured delays)\n"
                     (+ execution-time my/idle-loader-initial-delay
                        nominal-delays))))))

(let ((user-emacs-directory my/idle-profile--config-directory)
      ;; Keep profiling independent from --debug-init instrumentation.
      (init-file-debug nil)
      (startup-started (current-time)))
  (load (expand-file-name "early-init.el" user-emacs-directory) nil t)
  (load (expand-file-name "init.el" user-emacs-directory) nil t)
  ;; Normal graphical startup invokes this from `window-setup-hook'.  Batch
  ;; mode has no window setup, so invoke the same module pass explicitly.
  (my/load-config-modules)
  (when (timerp my/idle-loader--timer)
    (cancel-timer my/idle-loader--timer))
  (setq my/idle-loader--timer nil)
  (let ((queue (copy-tree my/idle-loader-forms)))
    ;; Do not let the real loader also consume the queue in this process.
    (setq my/idle-loader-forms nil)
    (princ (format "Idle loader profile\n===================\nEmacs   %s\nSystem  %s\nConfig  %s\nSetup   %.3fs\n"
                   emacs-version system-type user-emacs-directory
                   (float-time (time-subtract nil startup-started))))
    (my/idle-profile--print-module-times)
    (my/idle-profile--print-queue queue)
    (my/idle-profile--run-queue queue)))

;;; profile-idle-loader.el ends here
