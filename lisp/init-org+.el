;;; init-org+.el --- enhance org-mode                -*- lexical-binding: t; -*-

;; Copyright (C) 2025  DN

;; Author: DN <duan_n@outlook.com>
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
;; org-indent-mode hide leading stars, sometimes cursor become invisible.
;; (use-package math-preview
;;   :custom
;;   (math-preview-command "/opt/homebrew/bin/math-preview")
;;   (math-preview-scale 1.1)
;;   (math-preview-raise 0.2)
;;   (math-preview-margin '(1 . 0)))
;; (use-package org-xlatex
;;   :hook (org-mode . org-xlatex-mode))

(use-package cdlatex
  :load-path "packages/cdlatex/"
  :hook ((org-mode . org-cdlatex-mode)
         (LaTeX-mode . turn-on-cdlatex))
  :custom
  (cdlatex-auto-help-delay 0))

(use-package org-superstar
  :load-path "packages/org-superstar-mode/"
  :hook ((org-mode . org-superstar-mode)
	 (org-superstar-mode . org-indent-mode))
  :custom
  (org-hide-leading-stars t)
  (org-superstar-headline-bullets-list '("󰼏" "󰼐" "󰼑" "󰼒" "󰼓" "󰼔" "󰼕")))

;; 2024-09-14 还是放弃使用 org-modern，并没有太好用
;; org-modern can replace org-superstar
;; (use-package org-modern
;;   :hook (org-mode . global-org-modern-mode)
;;   :custom
;;   ;; (org-modern-checkbox nil)
;;   (org-modern-star 'replace)
;;   (org-modern-replace-stars "󰼏󰼐󰼑󰼒󰼓󰼔󰼕")
;;   (org-modern-table nil)
;;   ;; (org-modern-footnote nil)
;;   ;; (org-modern-internal-target nil)
;;   ;; (org-modern-radio-target nil)
;;   ;; (org-modern-progress nil)
;;   ;; (org-modern-tag nil)
;;   (org-modern-block-fringe nil)
;;   (org-modern-block-name nil)
;;   ;; (org-modern-timestamp nil)
;;   (org-modern-horizontal-rule nil)
;;   (org-modern-list '((?+ . "+")
;;                      (?- . "-")
;;                      (?* . "*")))
;;   (org-modern-checkbox '((?X . "󰄸")
;;                          (?- . "󱅶")
;;                          (?\s . "󰄶"))))

;; Third party packages related to org-mode
(use-package imenu-list
  :load-path "packages/imenu-list/"
  :hook (imenu-list-major-mode . (lambda ()
                                   (setq-local truncate-lines t)))
  :custom
  (imenu-list-position 'left)
  (imenu-list-mode-line-format '("%e" my/winum)))

(use-package form-feed
  :load-path "packages/form-feed/"
  :hook ((org-mode . form-feed-mode)
	 (emacs-news-mode . form-feed-mode)))

(defun my/toggle-visual-fill-center-imenu ()
  (interactive)
  (unless (featurep 'visual-fill-column)
    (require 'visual-fill-column))
  (setq visual-fill-column-center-text t)
  (if (or visual-fill-column-mode imenu-list-minor-mode)
      (progn
	(visual-fill-column-mode -1)
	(imenu-list-minor-mode -1))
    (progn
      (visual-fill-column-mode)
      (imenu-list-minor-mode))))

(global-set-key (kbd "s-M-z") #'my/toggle-visual-fill-center-imenu)

;; Instead of using `C-c C-x C-v' to toggle display inline image.
;; pixel-scroll-precision-mode enabled.
;; (use-package org-imgtog
;;   :load-path "packages/org-imgtog/"
;;   :hook (org-mode . org-imgtog-mode))

;; ox-pandoc
;; (with-eval-after-load 'ox
;;   (add-to-list 'load-path "~/.emacs.d/packages/ox-pandoc/")
;;   (add-to-list 'load-path "~/.emacs.d/packages/ht.el/")
;;   (require 'ox-pandoc))

(use-package plantuml
  :load-path "packages/plantuml-emacs/"
  :commands plantuml-org-to-mindmap-open plantuml-org-to-wbs-open)

(with-eval-after-load 'plantuml
  (setq plantuml-jar-path
        (concat (string-trim
                 (shell-command-to-string "readlink -f $(brew --prefix plantuml)"))
                "/libexec/plantuml.jar")))


(defun org-export-docx (input csl)
  (interactive "FInput file (Default is Buffer File):\nFCSL file (Default is chinese-gb7714-2005-numeric):")
  (let* ((base (or (file-name-sans-extension input) (buffer-file-name)))
	 (csl (or (expand-file-name "csl/chinese-gb7714-2005-numeric.csl" user-emacs-directory)))
	 (output (concat base ".docx")))
    (shell-command (format "pandoc %s -o %s --citeproc --csl %s" input output csl))))

;; https://www.reddit.com/r/emacs/comments/yjobc2/comment/iur16c7/
(defun nf/parse-headline (x)
  (plist-get (cadr x) :raw-value))

(defun nf/get-headlines ()
  (org-element-map (org-element-parse-buffer) 'headline #'nf/parse-headline))

(defun nf/link-to-headline ()
  "Insert an internal link to a headline."
  (interactive)
  (let* ((headlines (nf/get-headlines))
         (choice (completing-read "Headings: " headlines nil t))
         (desc (read-string "Description: " choice)))
    (org-insert-link buffer-file-name (concat "*" choice) desc)))

(use-package advance-words-count
  :load-path "packages/advance-words-count.el/"
  :bind ("M-=" . advance-words-count))

(defun my/insert-specified-datetree ()
  "Insert a datetree entry for a specified date."
  (interactive)
  (let* ((date (org-parse-time-string (org-read-date)))
         (year (nth 5 date))
         (month (nth 4 date))
         (day (nth 3 date)))
    (org-datetree-find-date-create (list month day year))
    (open-line 1)
    (forward-line 1)))


;; org-drawio
;; (use-package org-drawio
;;   :load-path "packages/org-drawio/"
;;   :commands (org-drawio-add
;;              org-drawio-open)
;;   :custom ((org-drawio-input-dir (abbreviate-file-name (expand-file-name "mindmap" my-galaxy)))
;;            (org-drawio-output-dir (abbreviate-file-name (expand-file-name "pictures" my-galaxy)))
;;            (org-drawio-output-page "0")
;; 		   (org-drawio-command-drawio "/Applications/draw.io.app/Contents/MacOS/draw.io")
;; 		   (org-drawio-command-pdf2svg "/opt/homebrew/bin/pdf2svg")
;; 		   (org-drawio-crop t)))

;; chatu
(use-package chatu
  :load-path "packages/chatu/"
  :hook (org-mode . chatu-mode)
  :commands (chatu-add
             chatu-open)
  :custom ((chatu-input-dir (abbreviate-file-name (expand-file-name "mindmap" my-galaxy)))
           (chatu-output-dir (abbreviate-file-name (expand-file-name "pictures" my-galaxy)))))

;;;###autoload
(defun my/org-chatu ()
  "Insert drawio image into org file with completion for knowledge name."
  (interactive)
  (let* ((folder (abbreviate-file-name (expand-file-name "mindmap" my-galaxy)))
         (files (directory-files folder nil "\\.drawio\\'"))
         (selected (completing-read "Choose Mindmap: " files))
         (name selected))
    (insert (format "#+chatu: :drawio \"%s\" :crop :nopdf\n" name))))


;; 没啥必要,纯文本的挺好
;; (use-package org-tag-beautify
;;   :hook (org-mode . org-tag-beautify-mode))

;; The contacts app in Phone is far better
;; (use-package org-contacts
;;   :custom
;;   (org-contacts-files `(,(expand-file-name "people/contacts.org" my-galaxy))))

;; edraw in Emacs
;; (add-to-list 'load-path "~/.emacs.d/packages/el-easydraw/")
;; (with-eval-after-load 'org
;;   (require 'edraw-org)
;;   (edraw-org-setup-default))
;; (use-package org-supertag
;;   :load-path "~/.emacs.d/packages/org-supertag/" "~/.emacs.d/packages/ht.el/"
;;   :hook (org-mode . org-supertag-setup))

;; (use-package org-xlatex
;;   :load-path "packages/org-xlatex/"
;;   :hook (org-mode . org-xlatex-mode))


(provide 'init-org+)
;;; init-org+.el ends here
