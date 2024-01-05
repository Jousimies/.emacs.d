;;; init-modeline.el --- Emacs Mode Line             -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Duan Ning

;; Author: Duan Ning <duan_n@outlook.com>
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

;; https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/prot-lisp/prot-modeline.el

;;; Code:


;; (setq mode-line-right-align-edge 'right-margin)

(defcustom prot-modeline-string-truncate-length 50
  "String length after which truncation should be done in small windows."
  :type 'natnum)

(defun prot-modeline--string-truncate-p (str)
  "Return non-nil if STR should be truncated."
  ;; (and (< (window-total-width) split-width-threshold)
  ;;      (> (length str) prot-modeline-string-truncate-length))
  (> (length str) prot-modeline-string-truncate-length))

(defun prot-modeline-string-truncate (str)
  "Return truncated STR, if appropriate, else return STR.
  Truncation is done up to `prot-modeline-string-truncate-length'."
  (if (prot-modeline--string-truncate-p str)
      (concat (substring str 0 prot-modeline-string-truncate-length) "...")
    str))

;; Major Mode
(defun my/modeline--major-mode ()
  (capitalize (string-replace "-mode" "" (symbol-name major-mode))))

(defvar-local my/modeline-major-mode
    '(:eval (propertize (my/modeline--major-mode) 'face `(:inherit font-lock-variable-name-face))))

;; File Name, truncate it if its length greater than prot-modeline-string-truncate-length.
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
    '(:eval (propertize (my/modeline-file-name) 'face 'bold)))

;; Readonly Mode
(defvar-local my/modeline-buffer-readonly
    '(:eval (when buffer-read-only
              (propertize " "
                          'face nil))))

;; Buffer Modified indicator
(defvar-local my/modeline-buffer-modified
    '(:eval (when (mode-line-window-selected-p)
              (propertize " * " 'face `(:inherit ,(if (buffer-modified-p) 'error nil))))))

;; Buffer Narrow
(defvar-local prot-modeline-narrow
    '(:eval
      (when (and (mode-line-window-selected-p)
                 (buffer-narrowed-p)
                 (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode)))
        (propertize " Narrow " 'face `(:inverse-video t))))
  "Mode line construct to report the multilingual environment.")

;; System Input Method status
(defvar-local my/modeline-input-method
    '(:eval (when (mode-line-window-selected-p)
              (propertize
               (if current-system-input-method
                   " ZH "
                 " EN ")
               'face `(:inherit ,(if current-system-input-method 'font-lock-string-face nil) :inverse-video t)))))

;; kbd Macro
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

;; Image size info
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

;; Date Info
(defvar-local my/modeline-date
    '(:eval (when (and (mode-line-window-selected-p) (> (window-width) 90))
              (propertize (format-time-string " %Y-%m-%d %a ") 'face `(:inherit success)))))

;; System coding and eol
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

;; Position info
(defun my/modeline--pdf-page ()
  (format " %d/%d " (eval '(pdf-view-current-page)) (pdf-cache-number-of-pages)))

(defvar-local my/modeline-position
    '(:eval (when (mode-line-window-selected-p)
              (if (derived-mode-p 'pdf-view-mode)
                  (propertize (my/modeline--pdf-page) 'face font-lock-string-face)
				(propertize (format " %%l:%%c/%d " (line-number-at-pos (point-max))) 'face nil)))))


;; Pomodoro
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

;; Clock Info
(defvar-local my/modeline-clock-info
    '(:eval (when (and (mode-line-window-selected-p) (org-clocking-p))
              (propertize (format " [%s](%s)"
                                  (org-duration-from-minutes
                                   (floor (org-time-convert-to-integer
                                           (org-time-since org-clock-start-time))
                                          60))
                                  org-clock-heading)
                          'face `(:inherit font-lock-builtin-face)))))

;; VSC Info
(defun my/modeline--vsc-state ()
  (let* ((backend (vc-backend buffer-file-name))
		 (state (vc-state buffer-file-name backend)))
	(cond ((eq state 'up-to-date) "√")
          ((eq state 'edited) "*")
          ((eq state 'added) "@")
          ((eq state 'needs-update) "￬")
          ((eq state 'needs-merge) "&")
          ((eq state 'unlocked-changes) "")
          ((eq state 'removed) "×")
          ((eq state 'conflict) "!")
          ((eq state 'missing) "?")
          ((eq state 'ignored) "-")
          ((eq state 'unregistered) "+")
          ((stringp state) (concat "#" state ":"))
          (t " "))))

(defvar-local my/modeline-vsc-info
	'(:eval (when (vc-backend (buffer-file-name))
			  (propertize (substring-no-properties vc-mode 1)
						  'face `(:inherit font-lock-builtin-face)))))

;; Battery status
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
;; Winum
(defvar-local my/winum
    '(:eval (propertize (format winum-format (winum-get-number-string)) 'face `(:inverse-video t ))))

;; eglot
(defvar-local prot-modeline-eglot
    `(:eval
      (when (and (featurep 'eglot) (mode-line-window-selected-p))
        '(eglot--managed-mode eglot--mode-line-format)))
  "Mode line construct displaying Eglot information.
Specific to the current window's mode line.")

(dolist (construct '(my/modeline-major-mode
                     my/modeline-buffer-indentification
					 prot-modeline-narrow
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
                     my/winum
					 prot-modeline-eglot))
  (put construct 'risky-local-variable t))

(setq-default mode-line-format
              '("%e"
                my/winum
                "​"
                ;; my/modeline-input-method
				;; "​"
				prot-modeline-narrow
                my/modeline-buffer-readonly
                my/modeline-buffer-modified
                my/modeline-file-name
                my/modeline-position
                my/modeline-image-info
                my/modeline-kbd-macro
                my/modeline-region-indicator
				prot-modeline-eglot
                "       "
                my/modeline-align-right
                (:eval (with-eval-after-load 'org-clock
                         my/modeline-clock-info))
                my/modeline-timer
				my/modeline-sys
				" "
                my/modeline-major-mode
				(vc-mode vc-mode)
                ))

(use-package keycast
  :load-path "packages/keycast/"
  :commands keycast-mode
  :config
  (setq keycast-mode-line-format "%2s%k%c%R")
  (setq keycast-mode-line-insert-after 'my/modeline-position)
  (setq keycast-mode-line-window-predicate 'mode-line-window-selected-p)
  (setq keycast-mode-line-remove-tail-elements nil)

  (dolist (input '(self-insert-command org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '(mouse-event-p mouse-movement-p mwheel-scroll))
    (add-to-list 'keycast-substitute-alist `(,event nil))))

(provide 'init-modeline)
;;; init-modeline.el ends here
