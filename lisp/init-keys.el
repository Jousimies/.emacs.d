;;; init-keys.el --- Key setting                     -*- lexical-binding: t; -*-

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

;; See here
;; https://github.com/positron-solutions/transient-showcase

;;; Code:

(transient-define-prefix my/note-menu ()
  "Note"
  [:description current-time-string
   ["New Note"
	("s" "With Signature" denote-signature :transient nil)
	("d" "To Subdirectory" denote-subdirectory :transient nil)
	("b" "Blog" my/new-blog :transient nil)
	("m" "Meeting" my/new-meeting :transient nil)
	("l" "Literature" my/literature-save :transient nil)]
   ["Denote Meta"
	("r" "Rename Note" denote-rename-file-using-front-matter :transient nil)
	("k" "Add Keyword" denote-keywords-add :transient nil)
	("K" "Remove Keyword" denote-keywords-remove :transient nil)]
   ["Denote Filter/Sort"
	("fp" "Prompt" denote-sort-dired :transient nil)
	("fs" "With Sigature" my/denote-sort-with-sigature :transient nil)
	("fi" "With Identifier" my/denote-sort-with-identifer :transient nil)
	("fk" "With Keywords" my/denote-sort-with-keywords :transient nil)]
   ["Extract Subtree"
	("x" "With Signature" my/denote-org-extract-subtree-with-signature :transient nil)
	("X" "To Subdirectory" my/denote-org-extract-subtree-to-subdirectory :transient nil)
	("a" "Annotation" ibooks-annot/extract-annotations-to-note :transient nil)]
   ["Reference"
	("p" "PDF" citar-open :transient nil)
	("e" "Entry" citar-open-entry :transient nil)
	("n" "Note" citar-open-notes :transient nil)]
   ["Misc"
	("s-n" " Consult Notes" consult-notes :transient nil)
	("SPC" "OCR" my/ocr :transient nil)]])

(global-set-key (kbd "s-n") #'my/note-menu)

(transient-define-prefix my/agenda-menu ()
  "GTD"
  [["Pomodoro"
	("t" "Toggle" my/pomodoro-toggle :transient nil)
	("SPC" "Pause or Continue" org-timer-pause-or-continue :transient nil)]
   ["Agenda"
	("a" "Agenda" org-agenda :transient nil)
	("b" "Book" my/book-agenda :transient nil)
	("T" "TODO" my/all-todo-agenda :transient nil)]
   ["Process & Engage"
	("x" "Process Inbox" org-gtd-process-inbox :transient nil)
	("@" "By Context" org-gtd-engage-grouped-by-context :transient nil)
	("<f12>" "Engage" org-gtd-engage :transient nil)]
   ["Clarify"
	("i" "Item" org-gtd-clarify-item :transient nil)
	("I" "Item: agenda" org-gtd-clarify-agenda-item :transient nil)]
   ["Review"
	("o" "Missed Appointments" org-gtd-oops :transient t)
	("m" "Missed Items" org-gtd-review-missed-items :transient t)
	("f" "Area of Focus" org-gtd-review-area-of-focus :transient t)]
   ["Review Stuck"
	("p" "Stuck Projects" org-gtd-review-stuck-projects :transient t)
	("c" "Calendar" org-gtd-review-stuck-calendar-items :transient t)
	("s" "Single Actions" org-gtd-review-stuck-single-action-items :transient t)
	("d" "Delegated" org-gtd-review-stuck-delegated-items :transient t)
	("w" "Waited: Incubated" org-gtd-review-stuck-incubated-items :transient t)
	("h" "Habit" org-gtd-review-stuck-habit-items :transient t)]
   ])

(global-set-key (kbd "<f12>") #'my/agenda-menu)

(transient-define-prefix my/dict-menu ()
  "Dictionary"
  [["SDCV"
	("i" "Input: POP" sdcv-search-input+ :transient nil)
	("I" "Input: Buffer" sdcv-search-input :transient nil)
	("p" "Point: POP" sdcv-search-pointer+ :transient nil)
	("P" "Point: Buffer" sdcv-search-pointer+ :transient nil)]
   ["Thesaurus"
	("s" "Synonyms" powerthesaurus-lookup-synonyms-dwim :transient nil)
	("a" "Antonyms" powerthesaurus-lookup-antonyms-dwim :transient nil)
	("r" "Related words" powerthesaurus-lookup-related-dwim :transient nil)
	("d" "Definitions" powerthesaurus-lookup-definitions-dwim :transient nil)
	("e" "Example sentences" powerthesaurus-lookup-sentences-dwim :transient nil)]
   ["Pronounce"
	("w" "Word" popweb-dict-say-word :transient nil)
	("S" "Sentence" emacs-azure-tts-sentence :transient nil)]
   ["Translate"
	("l" "Translate" gts-do-translate :transient nil)
	("L" "Translate: Yank" my/gts-do-translate :transient nil)
	("W" "Write Good" writegood-mode :transient nil)
	("h" "LSP Helper" lsp-bridge-toggle-sdcv-helper :transient nil)]
   ["OSX Dictionary"
	("o" "Input" osx-dictionary-search-input :transient nil)
	("x" "Pointer" osx-dictionary-search-pointer :transient nil)]
   ["Dictionary Overlay"
	("t" "Toggle" dictionary-overlay-toggle :transient nil)
	("g" "Refresh" dictionary-overlay-refresh-buffer :transient nil)
	("R" "Render buffer" dictionary-overlay-render-buffer :transient nil)
	("k" "Known Word" dictionary-overlay-mark-word-known :transient nil)
	("K" "Unknow Word" dictionary-overlay-mark-word-unknown :transient nil)]])

(global-set-key (kbd "C-c t") #'my/dict-menu)

(transient-define-prefix my/application-menu ()
  "Application"
  [["Elfeed"
	("e" "Elfeed" elfeed :transient nil)]
   ["Telega"
	("t" "Telega" telega :transient nil)]
   ["Email"
	("m" "Email" mu4e :transient nil)]
   ["BuiltIn"
	("c" "Calendar" calendar :transient nil)
	("a" "Calculator" calc :transient nil)]
   ])

(global-set-key (kbd "C-c a") #'my/application-menu)

(transient-define-prefix my/links-menu ()
  "Links"
  [["Org Link"
	("g" "Grab: Safari" my/link-grab :transient nil)
	("c" "Copy IDlink" my/copy-idlink :transient nil)
	("x" "Remove" jf/org-link-remove-link :transient nil)
	("s" "Store" org-store-link :transient nil)]
   ["Denote Link"
	("l" "Link" denote-link :transient nil)
	("a" "Add Links" denote-add-links :transient nil)
	("f" "Find Link" denote-find-link)
	("d" "DBlock: Insert Link" denote-org-dblock-insert-links :transient nil)
	("D" "DBlock: Insert Backlinks" denote-org-dblock-insert-backlinks :transient nil)]
   ["Denote Backlinks"
	("b" "Backlinks buffer" denote-backlinks :transient nil)
	("F" "Find backlink" denote-find-backlink :transient nil)]])

(global-set-key (kbd "s-l") #'my/links-menu)

(provide 'init-keys)
;;; init-keys.el ends here
