;; -*- lexical-binding: t; -*-

(defcustom prot-modeline-string-truncate-length 20
  "String length after which truncation should be done in small windows."
  :type 'natnum)

(defun prot-modeline--string-truncate-p (str)
  (and (< (window-total-width) split-width-threshold)
       (> (length str) prot-modeline-string-truncate-length)))

(defun prot-modeline-string-truncate (str)
  (if (prot-modeline--string-truncate-p str)
      (concat (substring str 0 prot-modeline-string-truncate-length) "...")
    str))

(defvar-local my/modeline-date
    '(:eval (when (and (mode-line-window-selected-p) (> (window-width) 90))
              (propertize (format-time-string " %Y-%m-%d %a ") 'face `(:inherit success)))))

(defvar-local my/modeline-time
    '(:eval (when (mode-line-window-selected-p)
              (propertize (format-time-string "%H:%M") 'face nil))))

;; (defvar-local my/modeline-timer
;;     '(:eval (when (and (mode-line-window-selected-p) (or org-timer-countdown-timer
;; 							 pomm-current-mode-line-string))
;;               (propertize (my/modeline--timer) 'face `(:inherit font-lock-constant-face)))))

(defun my/modeline--major-mode ()
  (capitalize (string-replace "-mode" "" (symbol-name major-mode))))
(defvar-local my/modeline-major-mode
    '(:eval (propertize (my/modeline--major-mode) 'face `(:inherit font-lock-variable-name-face))))

;; file name
(defun my/modeline-file-name ()
  (let ((name (or (buffer-file-name) (buffer-name))))
    (prot-modeline-string-truncate (file-name-nondirectory name))))
(defvar-local my/modeline-file-name
    '(:eval (propertize (my/modeline-file-name) 'face 'bold)))

;; position
(defvar-local my/modeline-position
    '(:eval (when (mode-line-window-selected-p)
              (if (derived-mode-p 'pdf-view-mode)
                  (propertize (format " %d/%d " (pdf-view-current-page) (pdf-cache-number-of-pages)) 'face font-lock-string-face)
		(propertize (format " %%l:%%c/%d " (line-number-at-pos (point-max))) 'face nil)))))

;; buffer
(defvar-local my/modeline-buffer-modified
    '(:eval (propertize " * " 'face `(:inherit ,(if (buffer-modified-p) 'error nil)))))

;; repeat
(defvar-local my/modeline-repeat
    '(:eval (when (and repeat-in-progress
                       (mode-line-window-selected-p))
              (propertize repeat-echo-mode-line-string 'face `(:inverse-video t)))))

;; macro
(defvar-local my/modeline-kbd-macro
    '(:eval
      (when (and (mode-line-window-selected-p) defining-kbd-macro)
	(propertize " KMacro " 'face `(:inherit font-lock-constant-face :inverse-video t)))))

;; region
(defvar-local my/modeline-region-indicator
    '(:eval (when (and (mode-line-window-selected-p) (use-region-p))
              (propertize
               (concat " L" (number-to-string (count-lines (region-beginning) (region-end)))
                       " W" (number-to-string (count-words (region-beginning) (region-end)))
                       " C" (number-to-string (abs (- (mark t) (point)))) " ")))))

;; system info
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

;; gtd
(defvar-local my/modeline-gtd
  '(:eval (when (featurep 'org-gtd)
	    (org-gtd-mode-lighter))))

;; org-clock
(defvar-local my/modeline-clock-info
    '(:eval (when (and (mode-line-window-selected-p) (org-clocking-p))
              (propertize (format " [%s](%s)"
                                  (org-duration-from-minutes
                                   (floor (org-time-convert-to-integer
                                           (org-time-since org-clock-start-time))
                                          60))
                                  org-clock-heading)
                          'face `(:inherit font-lock-builtin-face)))))

(dolist (construct '(my/modeline-major-mode
		     my/modeline-file-name
		     my/modeline-buffer-modified
		     my/modeline-kbd-macro
		     my/modeline-region-indicator
		     my/modeline-image-info
		     my/modeline-buffer-readonly
                     my/modeline-position
		     my/modeline-battery
		     my/modeline-sys
		     my/modeline-repeat
		     my/modeline-date
		     my/modeline-time
		     my/modeline-timer
		     my/modeline-clock-info
		     my/winum
		     my/modeline-gtd
		     my/telega))

  (put construct 'risky-local-variable t))

(setopt mode-line-right-align-edge 'right-margin)

(setopt mode-line-format
        '("%e"
	  (:eval (if current-input-method
                     (propertize (format " %s " current-input-method-title)
                                 'face 'font-lock-keyword-face)))
	  my/modeline-buffer-modified
          my/modeline-file-name
          "  "
	  "%I"
	  my/modeline-kbd-macro
	  my/modeline-position
	  my/modeline-region-indicator
	  mode-line-format-right-align
	  (:eval (with-eval-after-load 'org-clock
	   	   my/modeline-clock-info))
	  " "
	  my/modeline-gtd
	  my/modeline-sys
	  " "
	  my/modeline-major-mode
	  (project-mode-line project-mode-line-format)
	  (vc-mode vc-mode)
          " "))



(provide 'init-modeline)
