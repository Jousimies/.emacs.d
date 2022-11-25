;;; init-general.el --- General keybindings -*- lexical-binding: t no-byte-compile: t -*-

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

(when (require-package 'which-key)
  (which-key-mode 1)
  (setq which-key-idle-delay 0.1))

(require-package 'general)

;; Self defun
(defun my/emacs-config ()
  "My literate Emacs configuration."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun switch-to-message ()
  "Quick switch to `*Message*' buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun switch-to-scratch ()
  "Quick switch to `*Scratch*' buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun my/inbox-file ()
  "Open inbox file."
  (interactive)
  (find-file (expand-file-name "inbox/inbox.org" my-galaxy)))

(defun my/plan-file ()
  "Open plan file."
  (interactive)
  (find-file (expand-file-name "inbox/plan.org" my-galaxy)))

(defun my/index-file ()
    (interactive)
    (find-file (expand-file-name "roam/main/index.org" my-galaxy)))

(defun my/reflection-file ()
    (interactive)
    (find-file (expand-file-name "roam/main/reflection.org" my-galaxy)))

(defun my/finance-file ()
  "Open finance file."
  (interactive)
  (find-file (expand-file-name "finance/finance.bean" my-galaxy)))

;; ==============================files/buffer==============================
(general-define-key
 :states '(normal visual emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "f" '(:ignore t :wk "Files")
 "ff" '(find-file :wk "Find file")
 "fi" '(my/emacs-config :wk "Emacs configuration")
 "fr" '(recentf-open-files :wk "Recent files")

 "fb" '(browse-at-remote :wk "Browse remote")

 "fo" '(:ignore t :wk "Open file")
 "foe" '(my/elfeed-file :wk "Elfeed file")
 "foi" '(my/inbox-file :wk "Inbox file")
 "foI" '(my/index-file :wk "Index file")
 "fop" '(my/plan-file :wk "Plan file")
 "fof" '(my/finance-file :wk "Finance file")
 "for" '(my/reflection-file :wk "Reflection file")
 "fog" '(my/gtd-file :wk "GTD file"))

(general-define-key
 :keymaps '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "b" '(:ignore t :wk "Buffer/Bibtex")
 "bb" '(switch-to-buffer :wk "Switch buffer")
 "be" '(eval-buffer :wk "Eval buffer")
 "bk" '(kill-this-buffer :wk "Kill This Buffer")
 "bs" '(switch-to-scratch :wk "Swtich to scratch")
 "bm" '(switch-to-message :wk "Swtich to message")
 "br" '(dictionary-overlay-render-buffer :wk "Render buffer"))

;; ==============================Search==============================
(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "s" '(:ignore t :wk "Search")
 "sb" '(engine/search-bookdouban :wk "Book DouBan")
 "ss" '(engine/search-google :wk "Google")
 "sg" '(engine/search-github :wk "Github")
 "sy" '(engine/search-youtube :wk "Youtube")
 "sw" '(engine/search-wikipedia :wk "Wikipedia")
 "sm" '(engine/search-moviedouban :wk "Movie DouBan")
 "sz" '(engine/search-zhihu :wk "Zhihu")
 "sr" '(rg :wk "rg")
 "sl" '(consult-git-log-grep :wk "Git Log Grep"))

;; ==============================Language/Link==============================
(general-define-key
 :keymaps '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "l" '(:ignore t :wk "Link/Language")
 "lg" '(my/link-safari :wk "Grab Safari Link")
 "lr" '(jf/org-link-remove-link :wk "Link Remove")
 "lt" '(yt-set-time :wk "Set Youtube link time")

 "ll" '(gts-do-translate :wk "Translate")
 "lL" '(ingva-translate :wk "Lingva")

 "ld" '(osx-dictionary-search-pointer :wk "OSX dictionary")
 "lp" '(sdcv-search-pointer :wk "SDCV Point")
 "li" '(sdcv-search-input+ :wk "SDCV Input")

 "lk" '(dictionary-overlay-mark-word-unknown :wk "Mark word unknown")
 "lK" '(dictionary-overlay-mark-word-known :wk "Mark word known"))

;; ====================Applications==============================
(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "a" '(:ignore t :wk "Applications")
 "ae" '(elfeed-summary :wk "Elfeed")
 "at" '(telega :wk "Telega")
 "am" '(mu4e :wk "MAIL")
 "ac" '(calendar :wk "Calendar")
 "aC" '(calc :wk "calc"))

;; ==============================org==============================
(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "o" '(:ignore t :wk "Org")

 "oa" '(:ignore t :wk "Agenda")
 "oaa" '(my/org-agenda :wk "Agenda")
 "oat" '(org-todo-list :wk "Todo list")
 "oav" '(org-search-view :wk "View search")

 "ot" '(:ignore t :wk "Toggle")
 "ota" '(org-toggle-archive-tag :wk "Archive Tag")
 "oth" '(org-toggle-heading :wk "Heading")
 "oti" '(org-toggle-item :wk "Item")

 "oc" '(:ignore t :wk "Clock")
 "ocj" '(org-clock-goto :wk "Clock goto")
 "oci" '(org-clock-in :wk "Clock In")
 "oco" '(org-clock-out :wk "Clock Out")
 "ocl" '(org-clock-in-last :wk "Clock In Last")
 "ocr" '(org-mru-clock-select-recent-task :wk "Recent")
 "oca" '(alarm-clock-set :wk "Set alarm")
 "ocA" '(alarm-clock-list-view :wk "View alarm")

 "od" '(:ignore t :wk "Download")
 "odc" '(org-download-clipboard :wk "Download Clipboard")
 "ody" '(org-download-yank :wk "Download Yank")
 "odr" '(org-download-rename-last-file :wk "Rename last file")
 "odR" '(org-download-rename-at-point :wk "Rename point")

 "op" '(:ignore t :wk "Plantuml")
 "opm" '(plantuml-org-to-mindmap-open :wk "Mindmap")
 "ops" '(plantuml-org-to-wbs-open :wk "Work Breakdown Structure")

 "oh" '(boxy-headings :wk "Boxy heading"))

(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 :keymaps 'org-mode-map
 "RET" "C-c C-o"
 "nbm" '(orb-note-actions :wk "ORB Menu"))

(general-define-key
 :states '(normal visual insert emacs)
 :keymaps 'org-mode-map
 :prefix "SPC m"
 :non-normal-prefix "M-SPC m"
 "t" '(:ignore t :wk "Transclusion")
 "ta" '(org-transclusion-add :wk "Add")
 "tA" '(org-transclusion-add-all :wk "Add all")
 "tr" '(org-transclusion-remove :wk "Remove")
 "tR" '(org-transclusion-remove-all :wk "Remove all")
 "tg" '(org-transclusion-refresh :wk "Refresh")
 "tm" '(org-transclusion-make-from-link :wk "Make link")
 "to" '(org-transclusion-open-source :wk "Open source")
 "te" '(org-transclusion-live-sync-start :wk "Edit live"))

;; ==============================Notes==============================
(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "n" '(:ignore t :wk "Notes")

 "nr" '(org-roam-node-random :wk "Random node")

 "nf" '(org-roam-node-find :wk "Find node")
 "ni" '(org-roam-node-insert :wk "Insert node")

 "na" '(org-roam-alias-add :wk "Add alias")
 "nA" '(org-roam-alias-remove :wk "Remove alias")

 "nt" '(org-roam-tag-add :wk "Add tag")
 "nT" '(org-roam-tag-remove :wk "Remove tag")

 "nc" '(org-roam-dailies-capture-today :wk "Capture today")
 "nd" '(org-roam-dailies-goto-today :wk "Goto today")
 "nD" '(org-roam-dailies-goto-date :wk "Goto date")

 "nu" '(org-roam-ui-open :wk "Random node")
 "ns" '(consult-org-roam-search :wk "Search")
 "nv" '(consult-notes-org-roam-find-node-relation :wk "Node navigation")

 "nl" '(:ignore t :wk "Open Links")
 "nlb" '(consult-org-roam-backlinks :wk "Backlinks")
 "nlf" '(consult-org-roam-forward-links :wk "Forward Links")
 "nlr" '(gpc/open-node-roam-ref-url :wk "Ref link")

 "nb" '(:ignore t :wk "Bibtex")
 "nbp" '(citar-open-files :wk "Open files")
 "nbe" '(citar-open-entry :wk "Open entry")
 "nbn" '(citar-open-notes :wk "Open/Create note")
 "nbl" '(citar-open-links :wk "Open links")
 "nbc" '(citar-org-roam-cited :wk "Cited Roam Node")

 "nN" '(denote-open-or-create :wk "Denote open or create")
 "nM" '(consult-notes :wk "Find notes"))

;; ==============================Toggles==============================
(general-define-key
 :keymaps '(normal visual emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "t" '(:ignore t :wk "Toggles")

 "tp" '(toggle-proxy :wk "Proxy")

 "ti" '(bh/punch-in :wk "Punch In")
 "to" '(bh/punch-out :wk "Punch Out")

 "tm" '(demap-toggle :wk "Minimap")

 "tv" '(vterm :wk "vterm")

 "tz" '(writeroom-mode :wk "Zen mode"))

;; ==============================Emacs==============================
(general-define-key
 :keymaps '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "q" '(:ignore t :wk "Quit/Restart")
 "qR" '(restart-emacs :wk "Restart emacs")
 "qq" '(server-force-delete :wk "Server Delete")

 "e" '(:ignore t :wk "Enhance Emacs")
 "ep" '(epkg-describe-package :wk "Epkg describe package"))

;; ==============================specific mode map====================
(general-define-key
 :states 'normal
 :keymaps 'with-editor-mode-map
 "RET" "C-c C-c")

(general-define-key
 :keymaps 'org-capture-mode-map
 [remap evil-save-and-close]          'org-capture-finalize
 [remap evil-save-modified-and-close] 'org-capture-finalize
 [remap evil-quit]                    'org-capture-kill
 "RET" "C-c C-c"
 "SPC k" '(org-capture-kill :which-key "abort capture"))


(provide 'init-general)
;;; init-general.el ends here
