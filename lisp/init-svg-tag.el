;;; init-svg-tag.el --- replace keywords or regular expression with SVG tags  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Duan Ning

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

;;

;;; Code:

(use-package svg-tag-mode
  :load-path "packages/svg-lib/" "packages/svg-tag-mode/"
  :hook ((org-mode . svg-tag-mode)
		 (org-agenda-mode . svg-tag-mode))
  :config
  ;; https://github.com/rougier/svg-tag-mode
  ;; Show svg-tag-mode in org-agenda buffer
  (defun org-agenda-show-svg ()
    (let* ((case-fold-search nil)
           (keywords (mapcar #'svg-tag--build-keywords svg-tag--active-tags))
           (keyword (car keywords)))
      (while keyword
        (save-excursion
          (while (re-search-forward (nth 0 keyword) nil t)
            (overlay-put (make-overlay
                          (match-beginning 0) (match-end 0))
                         'display  (nth 3 (eval (nth 2 keyword)))) ))
        (pop keywords)
        (setq keyword (car keywords)))))
  (add-hook 'org-agenda-finalize-hook #'org-agenda-show-svg)

  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))
  (setq svg-lib-style-default (svg-lib-style-compute-default))
  (setq svg-tag-tags
		`(
		  ;; TODO keywords
		  ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :radius 1 :inverse t :margin 0))))
		  ("NEXT" . ((lambda (tag) (svg-tag-make "NEXT" :face 'success :radius 1 :inverse t :margin 0))))
		  ("CNCL" . ((lambda (tag) (svg-tag-make "CNCL" :face 'shadow :radius 1 :inverse t :margin 0))))
		  ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'button :radius 1 :inverse t :margin 0))))
		  ("WAIT" . ((lambda (tag) (svg-tag-make "WAIT" :face 'warning :radius 1 :inverse t :margin 0))))

		  ;; Priority
		  ("\\[#A\\]" . ((lambda (tag) (svg-tag-make tag :face 'org-level-1 :inverse t :beg 2 :end -1 :margin 0))))
		  ("\\[#B\\]" . ((lambda (tag) (svg-tag-make tag :face 'bold :inverse t :beg 2 :end -1 :margin 0))))
		  ("\\[#C\\]" . ((lambda (tag) (svg-tag-make tag :face 'org-priority :inverse t :beg 2 :end -1 :margin 0))))
		  ("\\[#D\\]" . ((lambda (tag) (svg-tag-make tag :face 'shadow :inverse t :beg 2 :end -1 :margin 0))))
		  ;; Tags
		  ;; 有问题，不能很好的处理多种情况
		  ;; ("\\(:[A-Z]+:\\)" . ((lambda (tag) (svg-tag-make tag :face 'org-tag :inverse t :margin 0 :beg 1 :end -1))))
		  ;; ("\\(:#[A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag :beg 2))))
          ;; ("\\(:#[A-Za-z0-9]+:\\)$" . ((lambda (tag) (svg-tag-make tag :beg 2 :end -1))))

		  ;; Citation likes [cite:@svenkinnunen1960]
		  ;; 与 Tags 一样，同样无法处理多种情况
		  ("\\[cite:@[A-Za-z0-9.]+\\]" . ((lambda (tag) (svg-tag-make tag :face 'org-cite-key :inverse t
																 :margin 0 :beg 7 :end -1))))

		  ;; https://github.com/rougier/svg-tag-mode/blob/main/examples/example-2.el
		  ;; Active date (with or without day name, with or without time)
          (,(format "\\(<%s>\\)" date-re) . ((lambda (tag)
											   (svg-tag-make tag :beg 1 :end -1 :margin 0))))
          (,(format "\\(<%s \\)%s>" date-re day-time-re) . ((lambda (tag)
															  (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
          (,(format "<%s \\(%s>\\)" date-re day-time-re) . ((lambda (tag)
															  (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

          ;; Inactive date  (with or without day name, with or without time)
          (,(format "\\(\\[%s\\]\\)" date-re) . ((lambda (tag)
												   (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
          (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) . ((lambda (tag)
																  (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
          (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) . ((lambda (tag)
																  (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))))
  )


(provide 'init-svg-tag)
;;; init-svg-tag.el ends here
