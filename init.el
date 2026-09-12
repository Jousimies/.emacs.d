;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(unless (fboundp 'decf)
  (defalias 'decf 'cl-decf)
  (defalias 'incf 'cl-incf))

(eval-and-compile
  (let ((source-lisp-directory
         (expand-file-name "lisp" user-emacs-directory))
        (build-lisp-directory
         (expand-file-name ".cache/packages-build/lisp"
                           user-emacs-directory)))
    (add-to-list 'load-path source-lisp-directory)
    (when (file-directory-p build-lisp-directory)
      (add-to-list 'load-path build-lisp-directory))))
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

      (setq my/config-modules
	    '(init-basic
	      init-modal
	      init-completion
	      init-edit
	      init-dired
	      init-buffer
	      init-org
	      init-note
	      init-bib
	      init-latex
	      init-gtd
	      init-reader
	      init-checker
	      init-prog
	      init-git
	      init-ai
	      init-finance))

      ;; Daemons do not run `window-setup-hook', so load on the first client
      ;; frame instead.  The loader itself guarantees a single pass.
      (add-hook (if (daemonp)
		    'server-after-make-frame-hook
		  'window-setup-hook)
		#'my/load-config-modules -90))

  (message "RUN PYTHON UPDATE_EMACS.PY IN TERMINAL!!!"))

;; Custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

;; Show startup times
(add-hook 'window-setup-hook
          (lambda ()
            (message "window-setup: %.3fs, after-init: %.3fs"
                     (float-time (time-subtract nil before-init-time))
                     (float-time (time-subtract after-init-time before-init-time)))))
