;; -*- lexical-binding: t; -*-

;; Package.el
(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca-no-symlink-mode)

(elpaca elpaca-use-package
	(elpaca-use-package-mode))

(setq use-package-always-ensure t)
(setq use-package-expand-minimally t)

;; on.el
(use-package on)

;; Auto detected coding system
(use-package unicad
  :config
  (unicad-mode 1))

;; (use-package gcmh-mode
;;   :vc (:url "https://gitlab.com/koral/gcmh.git"
;; 	    :rev "master")
;;   :hook (after-init . gcmh-mode)
;;   :custom
;;   (gc-cons-percentage 0.1)
;;   (gcmh-verbose nil)
;;   (gcmh-idle-delay 'auto)
;;   (gcmh-auto-idle-delay-factor 10)
;;   (gcmh-high-cons-threshold #x1000000))

(advice-add 'after-focus-change-function :after 'garbage-collect)
;; Server
(run-with-idle-timer 2 nil (lambda ()
			     (require 'server)
			     (unless (server-running-p)
			       (server-start))))

(elpaca transient)

;; Require configurations
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-vars)
(require 'init-util)
(require 'init-builtin)

(require 'init-modal)
(require 'init-edit)
(require 'init-ui)
(require 'init-modeline)
(require 'init-completion)
(require 'init-buffer)

(require 'init-org)
(require 'init-note)
(require 'init-bib)
(require 'init-latex)
(require 'init-reader)

(require 'init-gtd)

(require 'init-prog)
(require 'init-git)

(require 'init-misc)
;; Custom
(unless custom-file
  (load (setq custom-file (locate-user-emacs-file "custom.el")) t))

;; Show startup times
(add-hook 'window-setup-hook
          (lambda ()
            (message "window-setup: %.3fs, after-init: %.3fs"
                     (float-time (time-subtract nil before-init-time))
                     (float-time (time-subtract after-init-time before-init-time)))))
