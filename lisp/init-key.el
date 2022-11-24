;;; init-key.el --- One-key -*- lexical-binding: t no-byte-compile: t -*-

;; Copyright (C) 2022 Duan Ning

;; Author: Duan Ning <duan_n@outlook.com>
;; Version: 1.0.0
;; Keywords:

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'one-key)

(one-key-create-menu
 "Quit"
 '((("R" . "Restar") . restart-emacs))
 t)

(global-set-key (kbd "C-c q") 'one-key-menu-quit)

(one-key-create-menu
  "Open"
  '((("f" . "Finance") . my/finance-file)
    (("i" . "Inbox") . my/inbox-file)
    (("g" . "GTD") . my/gtd-file)
    (("e" . "Elfeed") . my/elfeed-file)
    (("I" . "Index") . my/index-file)
    (("r" . "Reflection") . my/reflection-file)
    (("p" . "Plan") . my/plan-file))
  t)

(one-key-create-menu
 "Files"
 '((("r" . "Recent") . consult-recent-file)
   (("i" . "Emacs Configuration") . my/emacs-config)
   (("o" . "Finance") . one-key-menu-open))
 t)

(global-set-key (kbd "C-c f") 'one-key-menu-files)

(one-key-create-menu
 "Buffer"
 '((("b" . "Buffer") . switch-to-buffer)
   (("m" . "Message") . switch-to-message)
   (("s" . "Scratch") . switch-to-scratch)
   (("e" . "Eval") . eval-buffer))

 t)
(global-set-key (kbd "C-c b") 'one-key-menu-buffer)

(one-key-create-menu
 "Search"
 '((("r" . "rg") . rg)
   (("l" . "Git log") . consult-git-log-grep))

 t)
(global-set-key (kbd "C-c s") 'one-key-menu-search)

(one-key-create-menu
 "Application"
 '((("e" . "Elfeed") . elfeed-summary)
   (("c" . "Calendar") . calendar))

 t)
(global-set-key (kbd "C-c a") 'one-key-menu-application)

(one-key-create-menu
 "Language/Link"
 '((("l" . "Translate") . gts-do-translate)
   (("L" . "Lingva") . lingva-translate)
   (("d" . "OSX dictionary") . osx-dictionary-search-pointer)
   (("k" . "Mark word unknown") . dictionary-overlay-mark-word-unknown)
   (("K" . "Mark word known") . dictionary-overlay-mark-word-known))

 t)
(global-set-key (kbd "C-c l") 'one-key-menu-language/link)
(one-key-create-menu
 "Clock"
 '((("j" . "Goto") . org-clock-goto)
   (("i" . "In") . org-clock-in)
   (("o" . "Out") . org-clock-out)
   (("l" . "Last") . org-clock-in-last)
   (("r" . "Recent") . org-mru-clock-select-recent-task))

 t)
(one-key-create-menu
 "Agenda"
 '((("a" . "Agenda") . my/org-agenda)
   (("t" . "Todo List") . org-todo-list)
   (("o" . "Out") . org-clock-out)
   (("v" . "View Search") . org-search-view))

 t)
(one-key-create-menu
 "Download"
 '((("c" . "clipboard") . org-download-clipboard)
   (("r" . "Rename last file") . org-download-rename-last-file)
   (("p" . "Rename at point") . org-download-rename-at-point))
 t)
(one-key-create-menu
 "Plantuml"
 '((("m" . "Mindmap") . plantuml-org-to-mindmap)
   (("s" . "Work Breakdown Structure") . org-download-rename-last-file))
 t)
(one-key-create-menu
 "ORG"
 '((("c" . "Clock") . one-key-menu-clock)
   (("a" . "Agenda") . one-key-menu-agenda)
   (("p" . "Download") . one-key-menu-plantuml)
   (("d" . "Download") . one-key-menu-download))
 t)
(global-set-key (kbd "C-c o") 'one-key-menu-org)

(one-key-create-menu
 "roam-link"
 '((("f" . "Forward links") . consult-org-roam-forward-links)
   (("b" . "Backlinks") . consult-org-roam-backlinks)
   (("r" . "Ref link") . gpc/open-node-roam-ref-url))
 t)
(global-set-key (kbd "C-c t") 'one-key-menu-toggle)

(one-key-create-menu
 "Notes"
 '((("r" . "Random Node") . org-roam-node-random)
   (("f" . "Find Node") . org-roam-node-find)
   (("i" . "Insert Node") . org-roam-node-insert)
   (("a" . "Add alias") . org-roam-alias-add)
   (("A" . "Remove alias") . org-roam-alias-remove)
   (("t" . "Add tag") . org-roam-tag-add)
   (("T" . "Remove tag") . org-roam-tag-remove)
   (("d" . "Goto today") . org-roam-dailies-goto-today)
   (("D" . "Goto Date") . org-roam-dailies-goto-date)
   (("u" . "Roam UI") . org-roam-ui-open)
   (("p" . "PDF") . citar-open-files)
   (("e" . "Entry") . citar-open-entry)
   (("n" . "Ref Notes") . citar-open-notes)
   (("s" . "Roam search") . consult-org-roam-search)
   (("v" . "Navigation") . consult-notes-org-roam-find-node-relation)
   (("l" . "Links") . one-key-menu-roam-link))
 t)
(global-set-key (kbd "C-c n") 'one-key-menu-notes)

(one-key-create-menu
 "Transclusion"
 '((("a" . "Add") . org-transclusion-add)
   (("A" . "Add all") . org-transclusion-add-all)
   (("r" . "Remove") . org-transclusion-remove)
   (("R" . "Remove all") . org-transclusion-remove-all)
   (("g" . "Refresh") . org-transclusion-refresh)
   (("m" . "Make link") . org-transclusion-make-from-link)
   (("o" . "Open Source") . org-transclusion-open-source)
   (("e" . "Edit live") . org-transclusion-live-sync-start))
 t)
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c t") 'one-key-menu-transclusion))
(provide 'init-key)
;;; init-key.el ends here
