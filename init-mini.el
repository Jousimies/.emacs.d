;;; init-mini.el --- Mininum emacs configuration.  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; Load path
;; https://github.com/seagle0128/.emacs.d/blob/master/init.el
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'.
Don't put large files in `site-lisp' directory, e.g. EAF.
Otherwise the startup will be very slow."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

;; Shell Path
;; https://www.emacswiki.org/emacs/ExecPath
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable.
To match that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
              "[ \t\n]*$" "" (shell-command-to-string
                      "$SHELL --login -c 'echo $PATH'"
                            ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)
;; Custom file
(setq custom-file (locate-user-emacs-file "custom.el"))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; https://github.com/purcell/emacs.d/blob/master/lisp/init-elpa.el
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (when (stringp min-version)
    (setq min-version (version-to-list min-version)))
  (or (package-installed-p package min-version)
      (let* ((known (cdr (assoc package package-archive-contents)))
             (best (car (sort known (lambda (a b)
                                      (version-list-<= (package-desc-version b)
                                                       (package-desc-version a)))))))
        (if (and best (version-list-<= min-version (package-desc-version best)))
            (package-install best)
          (if no-refresh
              (error "No version of %s >= %S is available" package min-version)
            (package-refresh-contents)
            (require-package package min-version t)))
        (package-installed-p package min-version))))

(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))

;; Define variables.
(defvar my-cloud "~/Nextcloud"
  "This folder is My cloud.")
(defvar my-galaxy (expand-file-name "L.Personal.Galaxy" my-cloud)
  "This folder stores all the plain text files of my life.")
(defvar website-directory "~/Nextcloud/L.Personal.Galaxy/website"
  "The source folder of my blog")
(defvar my/publish-directory "~/shuyi.github.io")

;;
(when (maybe-require-package 'no-littering)
  (require 'no-littering))

;; Epkg quick search package and jump to source repo.
(require-package 'epkg)
(require-package 'compat)
(require-package 'closql)
(require-package 'emacsql-sqlite)
(when (require-package 'epkg-marginalia)
  (with-eval-after-load 'marginalia
    (cl-pushnew 'epkg-marginalia-annotate-package
		(alist-get 'package marginalia-annotator-registry))))

;; Evil is better.
(when (require-package 'evil)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  (evil-mode 1))

;; General, evil-collection and which-key for keybindings.
(require-package 'general)
(require-package 'evil-collection)
(add-hook 'after-init-hook 'evil-collection-init)
(when (require-package 'which-key)
  (which-key-mode 1)
  (with-eval-after-load 'which-key
    (setq which-key-idle-delay 0.3)))


;; Server and restart-emacs
(server-mode)
(require-package 'restart-emacs)

(general-define-key
 :keymaps '(normal visual emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "q" '(:ignore t :wk "Quit/Restart")
 "qR" '(restart-emacs :wk "Restart emacs"))

;; Magit, a better git.
(when (maybe-require-package 'magit)
  (require-package 'git-timemachine))
(general-define-key
   :states 'normal
   :keymaps 'with-editor-mode-map
   "RET" "C-c C-c")

;; Recent file
(add-hook 'after-init-hook 'recentf-mode)
(with-eval-after-load 'recentf
  (setq recentf-max-saved-items 1000)
  (setq recentf-exclude nil))

(general-define-key
 :states '(normal visual emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "ff" '(find-file :wk "Find file")
 "fr" '(recentf-open-files :wk "Recent files"))

;;Undo-fu, undo-fu-session and vundo
(require-package 'undo-fu)
(require-package 'undo-fu-session)
(require-package 'vundo)

(add-hook 'after-init-hook 'global-undo-fu-session-mode)

(with-eval-after-load 'vundo
  (setq vundo-glyph-alist vundo-unicode-symbols))

(global-set-key (kbd "C-x u") 'vundo)

(when (maybe-require-package 'hungry-delete)
  (global-hungry-delete-mode 1))

;; Auto save
(require 'auto-save)
(auto-save-enable)
(setq auto-save-silent t)
(setq auto-save-delete-trailing-whitespace t)

;; Fonts and themes
(set-frame-font "Iosevka Fixed 16" nil t)
(if (display-graphic-p)
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
			charset (font-spec :family "Source Han Serif SC" :height 160))))

(require-package 'doom-themes)
(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'doom-nord-light t))
    ('dark (load-theme 'doom-nord t))))
(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

(toggle-frame-fullscreen)

(defun my/emacs-config ()
  "My literate Emacs configuration."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(general-define-key
 :keymaps '(normal visual emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "f" '(:ignore t :wk "Files")
 "fi" '(my/emacs-config :wk "Emacs configuration"))


(defun switch-to-message ()
  "Quick switch to `*Message*' buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun switch-to-scratch ()
  "Quick switch to `*Scratch*' buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(general-define-key
 :keymaps '(normal visual emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "b" '(:ignore t :wk "Buffer/Bibtex")
 "bb" '(switch-to-buffer :wk "Switch buffer")
 "be" '(eval-buffer :wk "Eval buffer")
 "bs" '(switch-to-scratch :wk "Swtich to scratch")
 "bm" '(switch-to-message :wk "Swtich to message"))

(general-define-key
 :states '(normal visual emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 ";e" '(epkg-describe-package :wk "Epkg"))

(provide 'init-mini)
;;; init-mini.el ends here