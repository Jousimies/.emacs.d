;;; init-ui.el --- User interface. -*- lexical-binding: t no-byte-compile: t -*-
;;; Code:
;;; Commentary:
;; Fonts and themes
(set-frame-font "Iosevka Fixed 16" nil t)
;; (set-frame-font "Latin Modern Mono 16" nil t)
(if (display-graphic-p)
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset (font-spec :family "Source Han Serif SC" :height 160))))

;; unicode font
(when (maybe-require-package 'unicode-fonts)
  (unicode-fonts-setup))

(require-package 'doom-themes)

;; Modus-theme is Emacs builtin.
(setq modus-themes-italic-constructs t)
(setq modus-themes-bold-constructs nil)
(setq modus-themes-region '(accented))
(setq modus-themes-lang-checkers '(straight-underline text-also intense))
(setq modus-themes-links '(neutral-underline))
(setq modus-themes-hl-line '(intense))
(setq modus-themes-paren-match '(accented intense))
(setq modus-themes-prompts '(intense background gray bold))
(setq modus-themes-completions '((matches . (extrabold intense))
                                 (selection . (underline))
                                 (popup . (intense))))
(setq modus-themes-org-blocks 'tinted-background)
(setq modus-themes-org-agenda '((header-block . (variable-pitch 1.2))
                                (habit . traffic-light)))
(setq modus-themes-headings '((t . (rainbow))))
(setq modus-themes-operandi-color-overrides '((bg-main . "#FFFFFF")
                                              (bg-dim . "#FFFFFF")
                                              (bg-hl-line . "#FFFFFF")
                                              (bg-active . "#FFFFFF")
                                              (bg-inactive . "#FFFFFF")
                                              (bg-tab-bar . "#FFFFFF")
                                              (bg-tab-active . "#FFFFFF")
                                              (bg-tab-inactive . "#FFFFFF")
                                              (blue . "#252321")
                                              (magenta-nuanced-bg . "#F2F0EF")))
(setq modus-themes-vivendi-color-overrides '((bg-main . "#1F1F1E")
                                             (bg-dim . "#1F1F1E")
                                             (bg-hl-line . "#1F1F1E")
                                             (bg-active . "#1F1F1E")
                                             (bg-inactive . "#1F1F1E")
                                             (bg-tab-bar . "#1F1F1E")
                                             (bg-tab-active . "#1F1F1E")
                                             (bg-tab-inactive . "#1F1F1E")
                                             (blue . "#FFFFFF")
                                             (magenta-nuanced-bg . "#343435")))

;; emacs-plus can switch themes by system.
(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'modus-operandi t))
    ('dark (load-theme 'doom-one t))))
;; (add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

;; ef-spring nice.
(require-package 'ef-themes)
(load-theme 'ef-spring t)

(setq ns-use-native-fullscreen nil)
(setq ns-use-fullscreen-animation nil)

(toggle-frame-maximized)

(when (maybe-require-package 'all-the-icons)
  (when (display-graphic-p)
    (add-hook 'after-init-hook (lambda ()
                                 (require 'all-the-icons))))

  (with-eval-after-load 'all-the-icons
    (let ((extension-icon-alist
           '(("bat"  all-the-icons-alltheicon "terminal" :face all-the-icons-lsilver)
             ("cmd"  all-the-icons-alltheicon "terminal" :face all-the-icons-lsilver)
             ("conf" all-the-icons-octicon "settings"    :v-adjust 0.0 :face all-the-icons-yellow)
             ("eln"  all-the-icons-octicon "file-binary" :v-adjust 0.0 :face all-the-icons-dsilver)
             ("epub" all-the-icons-faicon "book"         :height 1.0 :v-adjust -0.1 :face all-the-icons-green)
             ("exe"  all-the-icons-octicon "file-binary" :v-adjust 0.0 :face all-the-icons-dsilver)
             ("make" all-the-icons-fileicon "gnu"        :face all-the-icons-dorange)
             ("rss"  all-the-icons-octicon "rss"         :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange)
             ("toml" all-the-icons-octicon "settings"    :v-adjust 0.0 :face all-the-icons-yellow)
             ("tsx"  all-the-icons-fileicon "tsx"        :height 1.0 :v-adjust -0.1 :face all-the-icons-cyan-alt)
             ("xpm"  all-the-icons-octicon "file-media"  :v-adjust 0.0 :face all-the-icons-dgreen))))
      (dolist (icon extension-icon-alist)
        (add-to-list 'all-the-icons-extension-icon-alist icon)))

    (let ((regexp-icon-alist
           '(("\\.[bB][iI][nN]$"               all-the-icons-octicon "file-binary" :v-adjust 0.0 :face all-the-icons-yellow)
             ("^config$"                       all-the-icons-octicon "settings"    :v-adjust 0.0 :face all-the-icons-dorange)
             ("\\.\\(ba\\|z\\)shrc$"           all-the-icons-alltheicon "script"   :height 0.9 :face all-the-icons-dpink)
             ("\\.\\(bash\\|zsh\\)*_?profile$" all-the-icons-alltheicon "script"   :height 0.9 :face all-the-icons-dred)
             ("\\.\\(ba\\|z\\)sh_history$"     all-the-icons-alltheicon "script"   :height 0.9 :face all-the-icons-dsilver)
             ("\\.zshenv$"                     all-the-icons-alltheicon "script"   :height 0.9 :face all-the-icons-dred)
             ("Cask\\'"                        all-the-icons-fileicon "elisp"      :height 1.0 :v-adjust -0.2 :face all-the-icons-blue)
             ("NEWS$"                          all-the-icons-faicon "newspaper-o"  :height 0.9 :v-adjust -0.2)
             ("^Rakefile$"                     all-the-icons-alltheicon "ruby-alt" :face all-the-icons-red))))
      (dolist (icon regexp-icon-alist)
        (add-to-list 'all-the-icons-regexp-icon-alist icon)))

    (let ((mode-icon-alist
           '((xwidget-webkit-mode           all-the-icons-faicon "chrome"          :v-adjust -0.1 :face all-the-icons-blue)
             (bongo-playlist-mode           all-the-icons-material "queue_music"   :height 1.3 :face all-the-icons-green)
             (bongo-library-mode            all-the-icons-material "library_music" :height 1.1 :face all-the-icons-green)
             (simple-mpc-mode               all-the-icons-faicon "music"           :v-adjust -0.1 :face all-the-icons-green)
             (mingus-playlist-mode          all-the-icons-faicon "music"           :v-adjust -0.1 :face all-the-icons-green)
             (mingus-help-mode              all-the-icons-material "music_note"    :height 1.2 :face all-the-icons-green)
             (mingus-browse-mode            all-the-icons-material "library_music" :height 1.1 :face all-the-icons-green)
             (mingus-burn-mode              all-the-icons-material "queue_music"   :height 1.3 :face all-the-icons-green)
             (gnus-group-mode               all-the-icons-fileicon "gnu"           :face all-the-icons-silver)
             (gnus-summary-mode             all-the-icons-octicon "inbox"          :height 1.0 :v-adjust 0.0 :face all-the-icons-orange)
             (gnus-article-mode             all-the-icons-octicon "mail"           :height 1.1 :v-adjust 0.0 :face all-the-icons-lblue)
             (message-mode                  all-the-icons-octicon "mail"           :height 1.1 :v-adjust 0.0 :face all-the-icons-lblue)
             (diff-mode                     all-the-icons-octicon "git-compare"    :v-adjust 0.0 :face all-the-icons-lred)
             (flycheck-error-list-mode      all-the-icons-octicon "checklist"      :height 1.1 :v-adjust 0.0 :face all-the-icons-lred)
             (newsticker-mode               all-the-icons-faicon "rss-square"      :v-adjust -0.1 :face all-the-icons-orange)
             (newsticker-treeview-mode      all-the-icons-faicon "rss-square"      :v-adjust -0.1 :face all-the-icons-orange)
             (newsticker-treeview-list-mode all-the-icons-octicon "rss"            :height 1.1 :v-adjust 0.0 :face all-the-icons-orange)
             (newsticker-treeview-item-mode all-the-icons-octicon "rss"            :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange)
             (conf-mode                     all-the-icons-octicon "settings"       :v-adjust 0.0 :face all-the-icons-yellow)
             (conf-space-mode               all-the-icons-octicon "settings"       :v-adjust 0.0 :face all-the-icons-yellow)
             (gitconfig-mode                all-the-icons-octicon "settings"       :v-adjust 0.0 :face all-the-icons-dorange)
             (forge-topic-mode              all-the-icons-alltheicon "git"         :face all-the-icons-blue)
             (help-mode                     all-the-icons-faicon "info-circle"     :height 1.1 :v-adjust -0.1 :face all-the-icons-purple)
             (helpful-mode                  all-the-icons-faicon "info-circle"     :height 1.1 :v-adjust -0.1 :face all-the-icons-purple)
             (Info-mode                     all-the-icons-faicon "info-circle"     :height 1.1 :v-adjust -0.1)
             (cask-mode                     all-the-icons-fileicon "elisp"         :height 1.0 :v-adjust -0.2 :face all-the-icons-blue)
             (ein:notebooklist-mode         all-the-icons-faicon "book"            :face all-the-icons-lorange)
             (ein:notebook-mode             all-the-icons-fileicon "jupyter"       :height 1.2 :face all-the-icons-orange)
             (ein:notebook-multilang-mode   all-the-icons-fileicon "jupyter"       :height 1.2 :face all-the-icons-dorange)
             (nov-mode                      all-the-icons-faicon "book"            :height 1.0 :v-adjust -0.1 :face all-the-icons-green)
             (gfm-mode                      all-the-icons-octicon "markdown"       :face all-the-icons-lblue)
             (osx-dictionary-mode           all-the-icons-material "library_books" :face all-the-icons-lblue)
             (youdao-dictionary-mode        all-the-icons-material "library_books" :face all-the-icons-lblue)
             (fanyi-mode                    all-the-icons-material "library_books" :face all-the-icons-lblue))))
      (dolist (icon mode-icon-alist)
        (add-to-list 'all-the-icons-mode-icon-alist icon)))

    (when (maybe-require-package 'all-the-icons-completion)
      (add-hook 'after-init-hook 'all-the-icons-completion-mode)
      (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))))

;; Do not show curly at fringe.
(define-fringe-bitmap 'right-curly-arrow  [])
(define-fringe-bitmap 'left-curly-arrow  [])

;; Highline current line.
(add-hook 'after-init-hook 'global-hl-line-mode)

;; Keep line numbers inside a narrow
(setq-default display-line-numbers-widen t)

;; Menu bar
(add-hook 'org-mode-hook 'menu-bar--wrap-long-lines-window-edge)
(add-hook 'text-mode-hook 'menu-bar--display-line-numbers-mode-relative)
(add-hook 'prog-mode-hook 'menu-bar--display-line-numbers-mode-relative)

;; Show fill column indicator.
(setq-default fill-column 90)

;; only show fill indicator in prog mode.
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;; Show paren.
(setq show-paren-style 'parenthesis)
(setq show-paren-context-when-offscreen 'overlay)

(add-hook 'text-mode-hook 'show-paren-mode)

;;
(require-package 'rainbow-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)

;;
(when (maybe-require-package 'rainbow-delimiters)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Cursor.
(blink-cursor-mode -1)

;; Beacon
(when (maybe-require-package 'beacon)
  (setq-default beacon-color "Cyan")
  (setq-default beacon-lighter "")
  (setq-default beacon-size 30)
  (add-hook 'after-init-hook 'beacon-mode))

;; Symbol overlay
(when (maybe-require-package 'symbol-overlay)
  (dolist (hook '(prog-mode-hook html-mode-hook yaml-mode-hook conf-mode-hook))
    (add-hook hook 'symbol-overlay-mode))
  (with-eval-after-load 'symbol-overlay
    (define-key symbol-overlay-mode-map (kbd "M-i") 'symbol-overlay-put)
    (define-key symbol-overlay-mode-map (kbd "M-I") 'symbol-overlay-remove-all)
    (define-key symbol-overlay-mode-map (kbd "M-n") 'symbol-overlay-jump-next)
    (define-key symbol-overlay-mode-map (kbd "M-p") 'symbol-overlay-jump-prev)))


;; Page break lines
;; M-x quoted-insert RET C-l to insert page break line.
(when (maybe-require-package 'page-break-lines)
  (setq page-break-lines-max-width fill-column)
  (add-hook 'prog-mode-hook 'page-break-lines-mode))

(provide 'init-ui)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
