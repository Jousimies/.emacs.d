;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(unless (fboundp 'decf)
  (defalias 'decf 'cl-decf)
  (defalias 'incf 'cl-incf))

(eval-and-compile
  (add-to-list 'load-path
               (expand-file-name "lisp" user-emacs-directory)))
(require 'init-util)

(if (and (file-exists-p (expand-file-name "lisp/load-path-cache.el" user-emacs-directory))
	 (file-exists-p (expand-file-name "lisp/package-autoloads.el" user-emacs-directory)))
    (progn
      (require 'load-path-cache)
      (require 'package-autoloads)

      ;; Only load with --debug-init
      (when init-file-debug
	(require 'init-benchmark))

      (require 'init-vars)

      (require 'init-font)
      (require 'init-modeline)
      (require 'init-keys)

      (add-hook 'window-setup-hook
		(lambda ()
		  (require 'init-basic)
		  (require 'init-modal)
		  (require 'init-completion)
		  (require 'init-edit)
		  (require 'init-dired)
		  (require 'init-buffer)

		  (require 'init-org)
		  (require 'init-note)
		  (require 'init-bib)
		  (require 'init-latex)
		  (require 'init-gtd)
		  (require 'init-reader)

		  (require 'init-checker)

		  (require 'init-prog)
		  (require 'init-git)
		  (require 'init-ai)
		  )))

  (message "RUN PYTHON UPDATE_EMACS.PY IN TERMINAL!!!"))

;; Custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Show startup times
(add-hook 'window-setup-hook
          (lambda ()
            (message "window-setup: %.3fs, after-init: %.3fs"
                     (float-time (time-subtract nil before-init-time))
                     (float-time (time-subtract after-init-time before-init-time)))))
