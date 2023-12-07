;;; init-layout.el --- Emacs layout                  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Duan Ning

;; Author: Duan Ning <jousimies@DNs-Air.local>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defun toggle-tab-bar-mode ()
  (if (eq major-mode 'dashboard-mode)
	  (tab-bar-mode -1)
	(tab-bar-mode 1)))
(add-hook 'on-switch-buffer-hook #'toggle-tab-bar-mode)
(add-hook 'find-file-hook #'toggle-tab-bar-mode)
;; (add-hook 'after-init-hook #'tab-bar-mode)
(global-set-key (kbd "C-c b t") #'tab-switch)
(global-set-key (kbd "s-t") #'tab-new)
(global-set-key (kbd "s-w") #'tab-close)

(setopt tab-bar-new-tab-choice 'scratch-buffer
		tab-bar-close-button-show nil
		tab-bar-separator "​​"
		tab-bar-select-tab-modifiers '(super)
		tab-bar-tab-hints t)

(defvar my/tab-bar-right-string)
(defun my/tab-bar-time-update (&rest rest)
  (propertize (format-time-string "[%Y-%m-%d %a %H:%M]") 'face `(:inherit bold)))

(setq my/tab-bar-right-string '((:eval (my/tab-bar-time-update))))

(defun my/tab-bar-format-right ()
  `((global menu-item ,(format-mode-line my/tab-bar-right-string) ignore)))

(add-to-list 'my/tab-bar-right-string 'battery-mode-line-string t)

(setopt tab-bar-format '(tab-bar-format-menu-bar
						 tab-bar-format-history
						 tab-bar-format-tabs
						 tab-bar-separator
						 tab-bar-format-align-right
						 my/tab-bar-format-right))

(defun tab-bar-format-menu-bar ()
  "Produce the Menu button for the tab bar that shows the menu bar."
  `((menu-bar menu-item (propertize "  " 'face nil)
              tab-bar-menu-bar :help "Menu Bar")))

(defun my/tab-bar-tab-name-format-comfortable (tab i)
  (propertize (concat " " (tab-bar-tab-name-format-default tab i) " ")
              'face (funcall tab-bar-tab-face-function tab)))

(setq tab-bar-tab-name-format-function #'my/tab-bar-tab-name-format-comfortable)

;; modeline

;; https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/prot-lisp/prot-modeline.el
(defcustom prot-modeline-string-truncate-length 9
  "String length after which truncation should be done in small windows."
  :type 'natnum)

(defun prot-modeline--string-truncate-p (str)
  "Return non-nil if STR should be truncated."
  (and (< (window-total-width) split-width-threshold)
       (> (length str) prot-modeline-string-truncate-length)))

(defun prot-modeline-string-truncate (str)
  "Return truncated STR, if appropriate, else return STR.
  Truncation is done up to `prot-modeline-string-truncate-length'."
  (if (prot-modeline--string-truncate-p str)
      (concat (substring str 0 prot-modeline-string-truncate-length) "...")
    str))

(defun my/modeline--major-mode ()
  (capitalize (string-replace "-mode" "" (symbol-name major-mode))))

(defvar-local my/modeline-major-mode
    '(:eval (propertize (my/modeline--major-mode) 'face `(:inherit font-lock-variable-name-face))))

(defun my/modeline--buffer-name ()
  (when-let ((name (buffer-name)))
    (prot-modeline-string-truncate name)))

(defun my/modeline-buffer-name ()
  (let ((name (my/modeline--buffer-name)))
    (format "%s" name)))

(defun my/modeline--file-name ()
  (when-let ((name (buffer-file-name)))
    (prot-modeline-string-truncate (file-name-nondirectory name))))

(defun my/modeline-file-name ()
  (let ((name (my/modeline--file-name)))
    (if name
        (format "%s" name)
      (my/modeline-buffer-name))))

(defvar-local my/modeline-file-name
    '(:eval (when (mode-line-window-selected-p)
              (propertize (my/modeline-file-name) 'face 'bold))))

(defvar-local my/modeline-buffer-readonly
    '(:eval (when buffer-read-only
              (propertize " "
                          'face nil))))

(defvar-local my/modeline-buffer-modified
    '(:eval (when (mode-line-window-selected-p)
              (propertize " * " 'face `(:inherit ,(if (buffer-modified-p) 'error nil))))))

(defvar-local my/modeline-input-method
    '(:eval (when (mode-line-window-selected-p)
              (propertize
               (if current-system-input-method
                   " ZH "
                 " EN ")
               'face `(:inherit ,(if current-system-input-method 'font-lock-string-face nil) :inverse-video t)))))

(defvar-local my/modeline-kbd-macro
    '(:eval
      (when (and (mode-line-window-selected-p) defining-kbd-macro)
        (propertize " KMacro " 'face `(:inherit font-lock-constant-face :inverse-video t)))))

(defvar-local my/modeline-region-indicator
    '(:eval (when (and (mode-line-window-selected-p) (use-region-p))
              (propertize
               (concat "| L" (number-to-string (count-lines (region-beginning) (region-end)))
                       " W" (number-to-string (count-words (region-beginning) (region-end)))
                       " C" (number-to-string (abs (- (mark t) (point)))) " ")))))

(defun my/modeline--image-info ()
  (car (process-lines  "identify"  "-format"  "[%m %wx%h %b]" (buffer-file-name))))

(defvar-local my/modeline-image-info
    '(:eval (when (and (mode-line-window-selected-p) (or (eq major-mode 'image-mode)
                                                         (eq major-mode 'telega-image-mode)))
              (propertize (my/modeline--image-info) 'face font-lock-string-face))))

;; https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/prot-lisp/prot-modeline.el
(defun my/modeline--right-align-rest ()
  (format-mode-line
   `(""
     ,@(cdr (memq 'my/modeline-align-right mode-line-format)))))

(defun my/modeline--right-align-width ()
  (string-pixel-width (my/modeline--right-align-rest)))

(defun my/modeline--box-p ()
  "Return non-nil if the `mode-line' has a box attribute."
  (and (face-attribute 'mode-line :box)
       (null (eq (face-attribute 'mode-line :box) 'unspecified))))

(defun my/modeline--magic-number ()
  (let ((height (face-attribute 'mode-line :height nil 'default))
        (m-width (string-pixel-width (propertize "m" 'face 'mode-line))))
    (round height (* m-width (* height m-width 0.001)))))

(defvar-local my/modeline-align-right
    '(:eval
      (propertize
       " "
       'display
       `(space
         :align-to
         (- right
            right-fringe
            right-margin
            ,(ceiling
              (my/modeline--right-align-width)
              (string-pixel-width (propertize "m" 'face 'mode-line))))))))

(defvar-local my/modeline-date
    '(:eval (when (and (mode-line-window-selected-p) (> (window-width) 90))
              (propertize (format-time-string " %Y-%m-%d %a ") 'face `(:inherit success)))))

(defvar org-timer-countdown-timer nil)
(defun my/modeline--timer ()
  (when org-timer-countdown-timer
    (concat " " (org-timer-value-string))))

(defvar-local my/modeline-timer
    '(:eval (when (and (mode-line-window-selected-p) org-timer-countdown-timer)
              (propertize (my/modeline--timer) 'face `(:inherit error :inverse-video t)))))

(defvar-local my/modeline-time
    '(:eval (when (mode-line-window-selected-p)
              (propertize (format-time-string " %H:%MPM ") 'face `(:inherit success :inverse-video t)))))

(defun my/modeline--sys-coding-category ()
  (let ((sys (coding-system-plist buffer-file-coding-system)))
    (if (memq (plist-get sys :category)
              '(coding-category-undecided coding-category-utf-8))
        " UTF-8 "
      (upcase (symbol-name (plist-get sys :name))))))

(defun my/modeline--sys-coding-eol ()
  (let ((eol (coding-system-eol-type buffer-file-coding-system)))
    (pcase (coding-system-eol-type buffer-file-coding-system)
      (0 "LF")
      (1 "CRLF")
      (2 "CR")
      (_ ""))))

(defvar-local my/modeline-sys
    '(:eval (propertize (concat (my/modeline--sys-coding-category) (my/modeline--sys-coding-eol)) 'face nil)))

(defun my/modeline--pdf-page ()
  (format " %d/%d " (eval '(pdf-view-current-page)) (pdf-cache-number-of-pages)))

(defvar-local my/modeline-position
    '(:eval (when (mode-line-window-selected-p)
              (if (derived-mode-p 'pdf-view-mode)
                  (propertize (my/modeline--pdf-page) 'face font-lock-string-face)
				(propertize (format " %%l:%%c/%d " (line-number-at-pos (point-max))) 'face nil)))))

(defvar-local my/modeline-clock-info
    '(:eval (when (and (mode-line-window-selected-p) (org-clocking-p))
              (propertize (format " [%s](%s)"
                                  (org-duration-from-minutes
                                   (floor (org-time-convert-to-integer
                                           (org-time-since org-clock-start-time))
                                          60))
                                  org-clock-heading)
                          'face `(:inherit font-lock-builtin-face)))))

(defun my/modeline--battery-data ()
  (and battery-status-function
       (functionp battery-status-function)
       (funcall battery-status-function)))

(defun my/modeline--battery-status ()
  (cdr (assoc ?L (my/modeline--battery-data))))

(defun my/modeline--battery-percentage ()
  (car (read-from-string (or (cdr (assq ?p (my/modeline--battery-data))) "ERR"))))

(defun my/modeline--battery ()
  (let* ((charging? (string-equal "AC" (my/modeline--battery-status)))
         (percentage (my/modeline--battery-percentage)))
    (if charging?
        (format "󱐋%d%s" percentage "%%")
      (cond ((>= percentage 80) (format "󰁹%d%s" percentage "%%"))
            ((>= percentage 70) (format "󰂀%d%s" percentage "%%"))
            ((>= percentage 60) (format "󰁿%d%s" percentage "%%"))
            ((>= percentage 50) (format "󰁾%d%s" percentage "%%"))
            ((>= percentage 40) (format "󰁽%d%s" percentage "%%"))
            ((>= percentage 30) (format "󰁼%d%s" percentage "%%"))
            ((>= percentage 20) (format "󰁻%d%s" percentage "%%"))
            ((< percentage 20) (format "󰂎%d%s" percentage "%%"))))))

(defvar-local my/modeline-battery
    '(:eval (when (mode-line-window-selected-p)
              (propertize (my/modeline--battery) 'face `(:inherit ,(if (< (my/modeline--battery-percentage) 20)
                                                                       'error nil))))))

(defvar-local my/winum
    '(:eval (propertize (format winum-format (winum-get-number-string)) 'face `(:inverse-video t ))))

(dolist (construct '(my/modeline-major-mode
                     my/modeline-buffer-indentification
                     my/modeline-input-method
                     my/modeline-kbd-macro
                     my/modeline-region-indicator
                     my/modeline-align-right
                     my/modeline-file-name
                     my/modeline-buffer-readonly
                     my/modeline-buffer-modified
                     my/modeline-date
                     my/modeline-time
                     my/modeline-timer
                     my/modeline-sys
                     my/modeline-battery
                     my/modeline-position
                     my/modeline-image-info
                     my/modeline-clock-info
                     my/winum))
  (put construct 'risky-local-variable t))

(setq-default mode-line-format
              '("%e"
                my/winum
                "​"
                my/modeline-input-method
                my/modeline-buffer-readonly
                my/modeline-buffer-modified
                my/modeline-file-name
                my/modeline-position
                my/modeline-image-info
                my/modeline-kbd-macro
                my/modeline-region-indicator
                "       "
                my/modeline-align-right
                (:eval (with-eval-after-load 'org-clock
                         my/modeline-clock-info))
                my/modeline-timer
                my/modeline-major-mode
                my/modeline-sys))

;; icons
(use-package nerd-icons
  :load-path "packages/nerd-icons.el/"
  :commands nerd-icons-codicon nerd-icons-faicon nerd-icons-icon-for-file
  :config
  (setq nerd-icons-font-family "Hack Nerd Font Mono"))

(use-package nerd-icons-completion
  :load-path "packages/nerd-icons-completion/"
  :hook (minibuffer-setup . nerd-icons-completion-mode))

(use-package nerd-icons-dired
  :load-path "packages/emacs-nerd-icons-dired"
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :load-path "packages/nerd-icons-ibuffer/"
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;; frame

(use-package winum
  :load-path "packages/emacs-winum/"
  :hook (window-setup . winum-mode)
  :config
  (setq winum-auto-setup-mode-line nil)

  (defun my/winum-select (num)
    (lambda (&optional arg) (interactive "P")
      (if arg
          (winum-select-window-by-number (- 0 num))
        (if (equal num (winum-get-number))
            (winum-select-window-by-number (winum-get-number (get-mru-window t)))
          (winum-select-window-by-number num)))))

  (setq winum-keymap
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "C-M-0") 'winum-select-window-0-or-10)
          (dolist (num '(1 2 3 4 5 6 7 8 9) nil)
            (define-key map (kbd (concat "C-M-" (int-to-string num)))
                        (my/winum-select num)))
          map)))

(use-package perspective
  :load-path "packages/perspective-el/"
  :bind (("M-s-n" . persp-switch)
         ("M-s-w" . persp-kill))
  :custom
  (persp-mode-prefix-key (kbd "C-c z"))
  :hook ((emacs-startup . persp-mode)
         (kill-emacs . persp-state-save))
  :config
  (setq persp-state-default-file (expand-file-name "persp" cache-directory))
  (with-eval-after-load 'tab-bar
    (defun tab-bar-format-persp ()
      (setq global-mode-string (delete '(:eval (persp-mode-line)) global-mode-string))
      `((global menu-item ,(format-mode-line (persp-mode-line)) ignore)))
    (add-to-list 'tab-bar-format 'tab-bar-format-persp)))

(use-package popper
  :load-path "packages/popper/"
  :bind (("C-`" . popper-toggle)
         :map popper-mode-map
         ("M-<tab>" . popper-cycle)
         ("M-`" . popper-toggle-type))
  :hook (emacs-startup . popper-mode)
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$" "\\*Pp Eval Output\\*$"
          "\\*Compile-Log\\*"
          "\\*Completions\\*"
          "\\*Warnings\\*"
          "\\*Async Shell Command\\*"
          "\\*Apropos\\*"
          "\\*Backtrace\\*"
          "\\*Embark Actions\\*"
          "\\*Finder\\*"
          "\\*Kill Ring\\*"
          "\\*Go-Translate\\*"

          bookmark-bmenu-mode
          comint-mode
          compilation-mode
          help-mode helpful-mode
          tabulated-list-mode
          Buffer-menu-mode

          gnus-article-mode devdocs-mode
          grep-mode occur-mode rg-mode ag-mode pt-mode
          youdao-dictionary-mode osx-dictionary-mode fanyi-mode

          "^\\*Process List\\*" process-menu-mode
          list-environment-mode cargo-process-mode
          "^\\*EKG Capture"
          "^\\*Ibuffer\\*" ibuffer-mode
          "^\\*eshell.*\\*.*$" eshell-mode
          "^\\*shell.*\\*.*$"  shell-mode
          "^\\*terminal.*\\*.*$" term-mode
          "^\\*vterm.*\\*.*$"  vterm-mode
          "^\\*eldoc.*\\*.*$" eldoc-mode

          "\\*Flycheck errors\\*$" " \\*Flycheck checker\\*$"
          "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
          "\\*[Wo]*Man.*\\*$"
          "\\*ert\\*$" overseer-buffer-mode
          "\\*gud-debug\\*$"
          "\\*lsp-help\\*$" "\\*lsp session\\*$"
          "\\*quickrun\\*$"
          "\\*tldr\\*$"
          "\\*vc-.*\\*$"
          "^\\*elfeed-entry\\*$"
          "^\\*macro expansion\\**"

          "\\*TeX Help\\*"
          "^\\*denote-backlinks to "
          "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Org Note\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"))
  :config
  (setq popper-mode-line '(:eval (propertize "POP" 'face `(:inverse-video t))))
  ;; Enable indicator in minibuffer
  (defun my/popper--fit-window-height (win)
    "Determine the height of popup window WIN by fitting it to the buffer's content."
    (fit-window-to-buffer
     win
     (floor (frame-height) 2)
     (floor (frame-height) 3)))
  (setq popper-window-height #'my/popper--fit-window-height)
  ;; HACK: close popper with `C-g'
  (defun +popper-close-window-hack (&rest _)
    "Close popper window via `C-g'."
    (when (and (called-interactively-p 'interactive)
               (not (region-active-p))
               popper-open-popup-alist)
      (let ((window (caar popper-open-popup-alist)))
        (when (window-live-p window)
          (delete-window window)))))
  (advice-add #'keyboard-quit :before #'+popper-close-window-hack))

(use-package popper-echo
  :hook (popper-mode . popper-echo-mode))

(global-set-key (kbd "C-x C-b") #'ibuffer)
(add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)
  ;; https://github.com/roife/.emacs.d/blob/323536f51674ef68cad78f72eef31c8b49795518/core/init-ibuffer.el#L8
(defun +ibuffer-visit-buffer-in-popper ()
    (interactive)
    (if (window-parameter nil 'window-side)
        (let ((win (selected-window)))
          (ibuffer-visit-buffer-other-window)
          (delete-window win))
      (ibuffer-visit-buffer)))

(with-eval-after-load 'ibuffer
  (setopt ibuffer-default-sorting-mode 'major-mode)
  (define-key ibuffer-mode-map (kbd "RET") #'+ibuffer-visit-buffer-in-popper))

(use-package bufferlo
  :load-path "packages/bufferlo/"
  :bind (([remap switch-to-buffer] . bufferlo-switch-to-buffer))
  :hook (after-init . bufferlo-mode))

(provide 'init-layout)
;;; init-layout.el ends here
