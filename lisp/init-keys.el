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

(global-set-key (kbd "s-q") #'restart-emacs)

(defun my/copy-file-info (info-type)
  (interactive (list (completing-read "Copy file info (base/name/path/directory): "
                                      '("base" "name" "path" "directory"))))
  (when (buffer-file-name)
    (let ((file-info
           (pcase info-type
             ("base" (file-name-base (buffer-file-name)))
             ("name" (file-name-nondirectory (buffer-file-name)))
             ("path" (file-truename (buffer-file-name)))
             ("directory" (file-name-directory (buffer-file-name)))
             (other (user-error "Invalid info type")))))
      (kill-new file-info)
      (message "Copied %s: %s" info-type file-info))))

(defvar-keymap my/file-prefix-map
  :doc "Prefix map for file."
  "f" #'find-file
  "w" #'find-file-other-window
  "j" #'find-file-other-window-no-jump
  "p" #'find-file-at-point
  "t" #'find-file-other-tab
  "r" #'consult-recent-file
  "e" #'my/open-link-with-eww
  "F" #'macos-reveal-in-finder
  "s" #'macos-share
  "a" #'my/consult-find-attach
  "i" #'my/copy-file-info
  "d" #'consult-dir)

(keymap-set global-map "s-f" my/file-prefix-map)
(global-set-key (kbd "C-z") #'repeat)

(defvar-keymap my/window-prefix-map
  :doc "Keymap for windows"
  "u" #'winner-undo
  "r" #'winner-redo
  "h" #'windmove-left
  "l" #'windmove-right
  "j" #'windmove-down
  "k" #'windmove-up
  "p" #'previous-buffer
  "n" #'next-buffer
  "d" #'dired-sidebar-toggle-sidebar
  "m" #'switch-to-message
  "s" #'scratch-buffer)

(keymap-set global-map "M-o" my/window-prefix-map)

(transient-define-prefix my/bibtex-menu ()
  "References"
  [["Add"
	("a" "Zotra: Entry" zotra-add-entry :transient nil)
	("s" "SciHub" scihub :transient nil)
	("l" "Local PDF" citar-add-file-to-library :transient nil)]
   ["Citar Open"
	("o" "DWIM" citar-open :transient nil)
	("p" "PDF" citar-open-files :transient nil)
	("e" "Entry" citar-open-entry :transient nil)
	("n" "Note" citar-open-notes :transient nil)]
   ["Notes"
	("P" "PDF Files" citar-denote-open-files :transient nil)
	("R" "Open References" citar-denote-open-reference-entry :transient nil)
	("f" "Find Citation" citar-denote-find-citation :transient nil)]
   ["Citation"
	("i" "Insert" org-cite-insert :transient t)
	("c" "Insert: Tex" reftex-citation :transient t)]
   ["Export"
	("1" "Local Bibtex" citar-export-local-bib-file :transient nil)
	("2" "Bibtex to Endnote" my/bib2end :transient nil)]])

(global-set-key (kbd "s-b") #'my/bibtex-menu)

(transient-define-prefix my/note-menu ()
  "Note"
  [:description current-time-string
   ("s-n" " Consult Notes" consult-notes :transient nil)]
  [["New Note"
	("s" "With Signature" denote-signature :transient nil)
	("S" "To Subdirectory" denote-subdirectory :transient nil)
	("b" "Blog" my/new-blog :transient nil)
	("m" "Meeting" my/new-meeting :transient nil)
	("l" "Literature" my/literature-save :transient nil)
	("n" "Reference" citar-create-note :transient nil)]
   ["Denote Meta"
	("r" "Rename Note" denote-rename-file-using-front-matter :transient nil)
	("R" "Rename Keywords" denote-explore-rename-keyword :transient nil)
	("c" "Add Citekey" citar-denote-add-citekey :transient nil)
	("C" "Remove Citekey" citar-denote-remove-citekey :transient nil)
	("k" "Add Keyword" denote-keywords-add :transient nil)
	("K" "Remove Keyword" denote-keywords-remove :transient nil)]
   ["Denote Sort"
	("/p" "Prompt" denote-sort-dired :transient nil)
	("/s" "With Sigature" my/denote-sort-with-sigature :transient nil)
	("/i" "With Identifier" my/denote-sort-with-identifer :transient nil)
	("/k" "With Keywords" my/denote-sort-with-keywords :transient nil)
	("/d" "Days Ago" my/denote-sort-with-days :transient nil)
	]
   ["Denote Explore"
	("i" "Info" my/denote-info :transient nil)
	("t" "BarChart" denote-explore-keywords-barchart :transient t)
	("d" "Duplicate Identifier" denote-explore-identify-duplicate-identifiers :transient nil)
	("es" "Extract with Signature" my/denote-org-extract-subtree-with-signature :transient nil)
	("ed" "Extract to Subdirectory" my/denote-org-extract-subtree-to-subdirectory :transient nil)
	("eb" "Extract iBooks Annotation" ibooks-annot/extract-annotations-to-note :transient nil)]
   ["Misc"
	("M" "MindMap" plantuml-org-to-mindmap-open :transient nil)
	("w" "WBS" plantuml-org-to-mindmap-open :transient nil)
	("a" "Drawio Add" org-drawio-add :transient nil)
	("o" "Drawio Open" org-drawio-open :transient nil)
	("SPC" "OCR" my/ocr :transient nil)
	]])

(global-set-key (kbd "s-n") #'my/note-menu)

(transient-define-prefix my/links-menu ()
  "Links"
  ["Denote Backlinks"
	("b" "Buffer" denote-backlinks :transient nil)]
  [["Org Link"
	("c" "Copy IDlink" my/copy-idlink :transient nil)
	("i" "Insert" org-insert-link :transient nil)
	("s" "Store" org-store-link :transient nil)
	("t" "Display" org-toggle-link-display :transient nil)]
   ["Denote Insert"
	("l" "Link" denote-link :transient nil)
	("h" "Heading" denote-org-extras-link-to-heading :transient nil)
	("%" "Links: With Regexp" denote-add-links :transient nil)
	("d" "Links: DBlock" denote-org-extras-dblock-insert-links :transient nil)
	("D" "Backlinks: DBlock" denote-org-extras-dblock-insert-backlinks :transient nil)]
   ["Denote Links Roam"
	("e" "Explore Links" my/denote-find-link-other-window :transient t)
	("fb" "Find Backlinks" denote-find-backlink :transient nil)
	("fr" "References" citar-denote-find-reference :transient nil)]
   ["Misc"
	("g" "Grab: Safari" my/link-grab :transient nil)
	("x" "Remove" jf/org-link-remove-link :transient nil)]])

(global-set-key (kbd "s-l") #'my/links-menu)

(transient-define-prefix my/agenda-menu ()
  "GTD"
  [["Agenda"
	("a" "Agenda" org-agenda :transient nil)
	("b" "Book" my/book-agenda :transient nil)
	("t" "TODO" my/all-todo-agenda :transient nil)]
   ["Capture"
	("i" "Inbox" my/org-capture-inbox :transient nil)
	("l" "Inbox with link" my/org-capture-inbox-with-link :transient nil)
	("w" "Work Log" my/org-capture-work :transient nil)
	("r" "Review" my/org-capture-review :transient nil)]
   ["Process & Engage"
	("x" "Process Inbox" org-gtd-process-inbox :transient nil)
	("@" "By Context" org-gtd-engage-grouped-by-context :transient nil)
	("<f12>" "Engage" org-gtd-engage :transient nil)]
   ["Clarify"
	("c" "Item" org-gtd-clarify-item :transient nil)
	("C" "Item: agenda" org-gtd-clarify-agenda-item :transient nil)]
   ["Review"
	("o" "Missed Appointments" org-gtd-oops :transient t)
	("m" "Missed Items" org-gtd-review-missed-items :transient t)
	("f" "Area of Focus" org-gtd-review-area-of-focus :transient t)
	("s" "Stucks" my/gtd-stuck-menu :transient t)]
   ])

(transient-define-prefix my/gtd-stuck-menu ()
  "GTD Stuck"
  [[("p" "Projects" org-gtd-review-stuck-projects :transient t)]
   [("c" "Calendar" org-gtd-review-stuck-calendar-items :transient t)]
   [("s" "Single Actions" org-gtd-review-stuck-single-action-items :transient t)]
   [("d" "Delegated" org-gtd-review-stuck-delegated-items :transient t)]
   [("w" "Waited: Incubated" org-gtd-review-stuck-incubated-items :transient t)]
   [("h" "Habit" org-gtd-review-stuck-habit-items :transient t)]])

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

(global-set-key (kbd "M-s-t") #'my/dict-menu)

(transient-define-prefix my/pass-menu ()
  "Pass"
  ["Password"
   [("c" "Copy" password-store-copy :transient nil)]
   [("g" "Generate" password-store-generate :transient nil)]
   [("i" "insert" password-store-insert :transient nil)]
   [("e" "Edit" password-store-edit :transient nil)]
   [("r" "Rename" password-store-rename :transient nil)]
   [("d" "Delete" password-store-remove :transient nil)]])

(transient-define-prefix my/application-menu ()
  "Application"
  [["Finance"
	("g" "Generator" my/bean-generate :transient t)
	("f" "Fava" my/beancount-fava :transient nil)]]
  [[("e" "Elfeed" elfeed :transient nil)]
   [("t" "Telega" telega :transient nil)]
   [("m" "Email" mu4e :transient nil)]
   [("p" "Pass" my/pass-menu :transient nil)]
   [("s" "Search" my/search :transient nil)]
   [("c" "Calendar" calendar :transient nil)]
   [("r" "Calculator" calc :transient nil)]])

(global-set-key (kbd "M-s-a") #'my/application-menu)

(defvar-keymap my/org-prefix-map
  :doc "keymap for org."
  "i" #'org-clock-in
  "o" #'org-clock-out
  "g" #'org-clock-goto
  "p" #'pomm-third-time)

(keymap-set global-map "s-o" my/org-prefix-map)

(provide 'init-keys)
;;; init-keys.el ends here
