;; init-base.el --- Emacs Builtin. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(setq initial-major-mode 'fundamental-mode)
;; (setq inhibit-default-init t)
(setq inhibit-startup-screen t)
;; (setq inhibit-splash-screen t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(use-package server
  :hook (after-init . (lambda ()
                        (unless (server-running-p)
                          (server-start)))))

(with-eval-after-load 'org
  (add-to-list 'org-options-keywords "AUTO_TANGLE:")

  (defun my/auto-tangle ()
    "Auto export blog."
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (goto-char 0)
        (if (string-equal (car
                           (cdr
                            (car
                             (org-collect-keywords '("AUTO_TANGLE")))))
                          "t")
            (org-babel-tangle)))))

  (add-hook 'after-save-hook 'my/auto-tangle))

;; Variables defined in C source code
;; (setq ring-bell-function 'ignore)
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.05 nil 'invert-face 'mode-line)))
(setq tab-width 4)
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq use-short-answers t)
(setq read-process-output-max #x10000)
(setq create-lockfiles nil)
(setq recenter-redisplay nil)
(setq load-prefer-newer t)
(setq next-screen-context-lines 5)
(setq frame-inhibit-implied-resize t)
(setq inhibit-compacting-font-caches t)
(setq frame-resize-pixelwise t)
(setq inhibit-quit nil)
(setq fast-but-imprecise-scrolling t)
(setq scroll-preserve-screen-position 'always)
(setq auto-save-list-file-name nil)
(setq history-delete-duplicates t)
(setq bidi-display-reordering nil)
(setq read-buffer-completion-ignore-case t)
(setq completion-ignore-case t)
(setq delete-by-moving-to-trash t)
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(setq max-mini-window-height 10)
(setq redisplay-skip-fontification-on-input t)
(setq-default cursor-in-non-selected-windows nil)

;; Define some variables to facilitate the location of configuration files or related settings for specific systems.
(defvar mobile-document "~/Library/Mobile Documents/"
  "This folder contains documents in icloud.")

(defvar my-cloud "~/Nextcloud"
  "This folder is My cloud.")

;; L.Personal.Galaxy location may change, but folders in this directory never change.
(defvar my-galaxy (expand-file-name "L.Personal.Galaxy" my-cloud)
  "This folder stores all the plain text files of my life.")

(defvar website-directory (expand-file-name "blogs_source/" my-galaxy)
  "The source folder of my blog.")

(defvar my/web_archive (expand-file-name "web_archive/" my-galaxy)
  "The folder save web pages.")

(defvar my/reference-lists `(,(concat my-galaxy "/bibtexs/References.bib")
                             ,(concat my-galaxy "/bibtexs/Books.bib")))

(defvar no-littering-etc-directory
  (expand-file-name (convert-standard-filename "etc/") user-emacs-directory)
  "The directory where packages place their configuration files.
   This variable has to be set before `no-littering' is loaded.")

(defvar no-littering-var-directory
  (expand-file-name (convert-standard-filename "var/") user-emacs-directory)
  "The directory where packages place their persistent data files.
   This variable has to be set before `no-littering' is loaded.")

   ;;;###autoload
(defun no-littering-expand-etc-file-name (file)
  "Expand filename FILE relative to `no-littering-etc-directory'."
  (expand-file-name (convert-standard-filename file)
                    no-littering-etc-directory))

   ;;;###autoload
(defun no-littering-expand-var-file-name (file)
  "Expand filename FILE relative to `no-littering-var-directory'."
  (expand-file-name (convert-standard-filename file)
                    no-littering-var-directory))

(cl-letf (((symbol-function 'etc)
           (symbol-function #'no-littering-expand-etc-file-name))
          ((symbol-function 'var)
           (symbol-function #'no-littering-expand-var-file-name)))
  (make-directory no-littering-etc-directory t)
  (make-directory no-littering-var-directory t)
  (with-no-warnings ; many of these variables haven't been defined yet

 ;;; Built-in packages
    (setq abbrev-file-name (etc "abbrev.el"))
    (setq auto-insert-directory (etc "auto-insert/"))
    (setq org-clock-persist-file (var "org/clock-persist.el"))
    (setq org-id-locations-file (var "org/id-locations.el"))
    (setq org-persist-directory (var "org/persist/"))
    (setq org-publish-timestamp-directory (var "org/timestamps/"))
    (setq auto-save-list-file-prefix (var "auto-save/sessions/"))
    (setq bookmark-default-file (var "bookmark-default.el"))
    (setq url-cache-directory (var "url/cache/"))
    (setq url-configuration-directory (var "url/"))
    (setq url-cookie-file (var "url/cookies.el"))
    (setq url-history-file (var "url/history.el"))
    (setq multisession-directory (var "multisession/"))
    (setq transient-history-file (var "transient/history.el"))
    (setq transient-levels-file (etc "transient/levels.el"))
    (setq transient-values-file (etc "transient/values.el"))
    (setq savehist-file (var "savehist.el"))
    (setq save-place-file (var "save-place.el"))
    (setq recentf-save-file (var "recentf-save.el"))
    (setq image-dired-db-file (var "image-dired/db.el"))
    (setq image-dired-dir (var "image-dired/"))
    (setq image-dired-gallery-dir (var "image-dired/gallery/"))
    (setq image-dired-temp-image-file (var "image-dired/temp-image"))
    (setq image-dired-temp-rotate-image-file (var "image-dired/temp-rotate-image"))

    (eval-after-load 'org `(make-directory ,(var "org/") t))
 ;;; Third-part packages
    (setq request-storage-directory (var "request/storage/"))
    (setq bard-cookie-token-path (var "bard_cookie_token.txt"))
    (setq tabspaces-session-file (var "tabspaces-session.eld"))
    (setq undo-fu-session-directory (var "undo-fu-session/"))
    (setq nov-save-place-file (var "nov-save-place.el"))
    (setq epkg-repository (var "epkgs"))
    (setq persp-state-default-file (var "persp.el"))
    (setq popweb-config-location (var "popweb"))
    (setq wg-session-file (var ".emacs_workgroups"))))

(use-package gcmh
  :load-path "packages/gcmh"
  :hook ((after-init . gcmh-mode)
         (focus-out . garbage-collect))
  :config
  (setq gc-cons-percentage 0.1)
  (setq gcmh-idle-delay 'auto)
  (setq gcmh-auto-idle-delay-factor 10)
  (setq gcmh-high-cons-threshold #x1000000))

(use-package url-vars
  :defer t
  :config
  (setq url-proxy-services
        '(("http" . "127.0.0.1:7891")
          ("https" . "127.0.0.1:7891")
          ("socks" . "127.0.0.1:7891")
          ("no_proxy" . "0.0.0.0"))))

(use-package simple
  :bind (("C-c b s" . scratch-buffer)
         ("C-h" . delete-backward-char)
         ("M-h" . backward-kill-word)
         ("<f1>" . help-command))
  :hook ((prog-mode . column-number-mode)
         (text-mode . size-indication-mode)
         (text-mode . turn-on-visual-line-mode)
         (org-mode . turn-on-visual-line-mode)
         (LaTeX-mode . turn-on-visual-line-mode))
  :init
  (setq-default indent-tabs-mode nil)
  :config
  ;; (setq read-extended-command-predicate #'command-completion-default-include-point)
  (setq completion-auto-select 'second-tab)
  (setq mark-ring-max 128)
  (setq kill-do-not-save-duplicates t)
  (setq kill-ring-max (* kill-ring-max 2))
  (setq async-shell-command-display-buffer nil))

(use-package files
  :hook ((after-init . auto-save-visited-mode)
         (before-save . auto-save-delete-trailing-whitespace-except-current-line))
  :config
  (setq trash-directory "~/.Trash")
  (setq auto-save-default nil)
  (setq auto-save-visited-interval 1)
  (setq save-silently t)
  (setq large-file-warning-threshold nil)
  (setq confirm-kill-processes nil)
  (setq confirm-kill-emacs nil)
  (setq make-backup-files nil)
  ;; (setq backup-directory-alist '(("." . "~/.emacs.d/cache/backups")))
  (setq view-read-only t)
  (setq kill-read-only-ok t)
  ;; https://emacsredux.com/blog/2022/06/12/auto-create-missing-directories/
  (defun my/auto-create-missing-dirs ()
    (let ((target-dir (file-name-directory buffer-file-name)))
      (unless (file-exists-p target-dir)
        (make-directory target-dir t))))
  (add-to-list 'find-file-not-found-functions #'my/auto-create-missing-dirs)

  (defun auto-save-delete-trailing-whitespace-except-current-line ()
    (interactive)
    (let ((begin (line-beginning-position))
          (end (point))
          (buffername (buffer-name (buffer-base-buffer))))
      (when (not (or (string-prefix-p "inbox" buffername)
                     (string-match-p "^[0-9]" buffername)))
        (save-excursion
          (when (< (point-min) begin)
            (save-restriction
              (narrow-to-region (point-min) (1- begin))
              (delete-trailing-whitespace)))
          (when (> (point-max) end)
            (save-restriction
              (narrow-to-region end (point-max))
              (delete-trailing-whitespace))))))))

(use-package ffap
  :bind ("C-c f f" . find-file-at-point))

(defun my/open-with-default-browse ()
  (interactive)
  (browse-url-default-browser (ffap-url-at-point)))

(global-set-key (kbd "s-<return>") 'my/open-with-default-browse)

(use-package message
  :bind ("C-c b m" . switch-to-message)
  :config
  (defun switch-to-message ()
    "Quick switch to `*Message*' buffer."
    (interactive)
    (switch-to-buffer "*Messages*"))
  (setq message-kill-buffer-on-exit t)
  (setq message-kill-buffer-query nil)
  (setq message-sendmail-envelope-from 'header)
  (setq message-kill-buffer-query nil)
  (setq message-sendmail-extra-arguments '("-a" "outlook")))

(use-package calc
  :bind ("C-c m c" . calc)
  :hook ((calc-trail-mode . (lambda ()
                              (setq-local mode-line-format nil)))
         (calc-mode . (lambda ()
                        (setq-local mode-line-format nil))))
  :config
  (setq calc-window-height 15))

(use-package so-long
  :hook (after-init . global-so-long-mode))

(use-package prog-mode
  :hook ((prog-mode . prettify-symbols-mode)
         (LaTeX-mode . prettify-symbols-mode))
  :config
  (setq prettify-symbols-alist '(("lambda" . ?λ)
                                 ("function" . ?𝑓))))

(use-package outline
  :hook (prog-mode . outline-minor-mode))

(use-package pixel-scroll
  :hook (after-init . pixel-scroll-precision-mode))

(use-package doc-view
  :defer t
  :config
  (setq doc-view-mupdf-use-svg t)
  (setq doc-view-imenu-flatten t)
  (setq doc-view-continuous t))

(use-package abbrev
  :hook ((org-mode . abbrev-mode)
         (LaTeX-mode . abbrev-mode)))

(use-package cursor-sensor
  :hook (minibuffer-setup . cursor-intangible-mode))

(use-package midnight
  :hook (after-init . midnight-mode))

(use-package word-wrap-mode
  :hook (org-mode . word-wrap-whitespace-mode))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-default-sorting-mode 'major-mode))

(use-package ibuf-ext
  :hook (ibuffer-mode . ibuffer-auto-mode))

(use-package time
  :hook (after-init . display-time-mode))

(use-package battery
  :hook (after-init . display-battery-mode))

(use-package gc-buffers
  :load-path "packages/emacs-gc-buffers"
  :hook (after-init . gc-buffers-mode))

(setq browse-url-browser-function 'xwidget-webkit-browse-url)
(use-package xwidget
  :bind (:map xwidget-webkit-mode-map
              ("o" . my/xwidget-open-with-default-browse))
  :init
  (add-to-list 'display-buffer-alist '("^\\*xwidget"
                                       (display-buffer-in-tab)))
  :config
  (defun my/xwidget-open-with-default-browse ()
    (interactive)
    (browse-url-default-browser (xwidget-webkit-uri (xwidget-webkit-current-session))))
  (setq xwidget-webkit-download-dir my/web_archive))

;;;###autoload
(defun my/save-xwidget-to-webarchive ()
    (interactive)
    (let ((session (xwidget-webkit-current-session)))
      (xwidget-webkit-save-as-file
       (xwidget-webkit-uri session)
       'html
       (concat (xwidget-webkit-title session) ".html"))))

(provide 'init-base)
;;; init-base.el ends here.
