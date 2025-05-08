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
;; 如果有内置的按键，优先使用内置的按键。
;; 如果内置的按键不好用，则自己定义。
;; C-x for global keybinding
;; C-c for local keybinding

;; See here for transient use.
;; https://github.com/positron-solutions/transient-showcase

;;; Code:
(require 'transient)

(when IS-WINDOWS
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-pass-rwindow-to-system nil)
  (setq w32-lwindow-modifier 'super)
  (setq w32-rwindow-modifier 'super)
  (setq w32-pass-alt-to-system nil)
  (w32-register-hot-key [s-])
  (w32-register-hot-key [c-])
  (setq w32-recognize-altgr nil))

;; (global-set-key (kbd "C-h") #'delete-backward-char)
;; (global-set-key (kbd "s-h") #'help-command)
;; (global-set-key (kbd "M-h") #'backward-kill-word)

(defvar-keymap my/file-prefix-map
  :doc "Prefix map for file."
  "f" #'find-file
  "b" #'ibooks-annot/open-book-with-ibooks
  "w" #'find-file-other-window
  "j" #'find-file-other-window-no-jump
  "p" #'find-file-at-point
  "t" #'find-file-other-tab
  "r" #'my/open-recentf
  "e" #'my/open-link-with-eww
  "F" #'macos-reveal-in-finder
  "s" #'macos-share
  "a" #'my/consult-find-attach
  "c" #'my/copy-file-info
  "i" #'my/denote-info
  "d" #'consult-dir
  "z" #'persp-state-load
  "u" #'update-org-attach-property)

;; (defvar-keymap my/new-note-prefix-map
;;   :doc "Prefix map for new note."
;;   "a" #'ibooks-annot/extract-annotations-to-note
;;   "n" #'denote
;;   "c" #'denote-fz-insert-child-here
;;   "s" #'denote-fz-insert-sibling-here
;;   "b" #'my/new-blog
;;   "e" #'denote-org-extras-extract-org-subtree
;;   "m" #'my/new-meeting
;;   "l" #'my/literature-save
;;   "r" #'citar-create-note
;;   "k" #'denote-rename-file-keywords
;;   "t" #'denote-rename-file-title
;;   "o" #'my/ocr)

(defvar-keymap my/sort-note-prefix-map
  :doc "Prefix map for sort note."
  "s" #'denote-sequence-find-dired-siblings
  "c" #'denote-sequence-find-dired-children
  "C" #'denote-sequence-find-dired-all-children
  "a" #'denote-sequence-dired
  "p" #'denote-sequence-find-dired-parent
  "P" #'denote-sequence-find-dired-all-parents
  "k" #'my/denote-sort-with-keywords
  "d" #'my/denote-sort-period-week)

(transient-define-prefix my/bibtex-menu ()
  "References"
  [["Add"
    ("a" "Zotra: Entry" zotra-add-entry :transient nil)
    ("s" "SciHub" scihub :transient nil)
    ("l" "Local PDF" citar-add-file-to-library :transient nil)]
   ["Citar"
    ("o" "Open" citar-open :transient nil)
    ("f" "Files" citar-open-files :transient nil)
    ("e" "Entry" citar-open-entry :transient nil)
    ("n" "Note" citar-open-notes :transient nil)]
   ["Citar Denote"
    ("k" "Add Citekey" citar-denote-add-citekey :transient nil)
    ("K" "Remove Citekey" citar-denote-remove-citekey :transient nil)
    ("F" "Files" citar-denote-open-files :transient nil)
    ("E" "Entry" citar-denote-open-reference-entry :transient nil)]
   ["Citation"
    ("c" "Find" citar-denote-find-citation :transient nil)
    ("i" "Insert" citar-insert-citation :transient t)]
   ["Export"
    ("1" "Local Bibtex" citar-export-local-bib-file :transient nil)
    ("2" "Bibtex to Endnote" my/bib2end :transient nil)]])

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

(transient-define-prefix my/agenda-menu ()
  "GTD"
  [["Agenda"
    ("a" "Agenda" org-agenda :transient nil)
    ("b" "Book" my/book-agenda :transient nil)
    ("t" "TODO" my/all-todo-agenda :transient nil)]
   ["Process & Engage"
    ("x" "Process Inbox" org-gtd-process-inbox :transient nil)
    ("@" "By Context" org-gtd-engage-grouped-by-context :transient nil)
    ("<f12>" "Engage" my/org-gtd-engage :transient nil)]
   ["Clarify"
    ("c" "Item" org-gtd-clarify-item :transient nil)
    ("C" "Item: agenda" org-gtd-clarify-agenda-item :transient nil)]
   ["Review"
    ("o" "Missed Appointments" org-gtd-oops :transient t)
    ("m" "Missed Items" org-gtd-review-missed-items :transient t)
    ("f" "Area of Focus" org-gtd-review-area-of-focus :transient t)
    ("s" "Stucks" my/gtd-stuck-menu :transient t)]])

(transient-define-prefix my/gtd-stuck-menu ()
  "GTD Stuck"
  [[("p" "Projects" org-gtd-review-stuck-projects :transient t)]
   [("c" "Calendar" org-gtd-review-stuck-calendar-items :transient t)]
   [("s" "Single Actions" org-gtd-review-stuck-single-action-items :transient t)]
   [("d" "Delegated" org-gtd-review-stuck-delegated-items :transient t)]
   [("w" "Waited: Incubated" org-gtd-review-stuck-incubated-items :transient t)]
   [("h" "Habit" org-gtd-review-stuck-habit-items :transient t)]])

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
   ["Translate"
    ("w" "Speack" gt-do-speak :transient nil)
    ("l" "Translate" gt-do-translate :transient nil)
    ("W" "Write Good" writegood-mode :transient nil)
    ("h" "LSP Helper" lsp-bridge-toggle-sdcv-helper :transient nil)]
   ;; ["OSX Dictionary"
   ;;  ("o" "Input" osx-dictionary-search-input :transient nil)
   ;;  ("x" "Pointer" osx-dictionary-search-pointer :transient nil)]
   ;; ["Dictionary Overlay"
   ;;  ("t" "Toggle" dictionary-overlay-toggle :transient nil)
   ;;  ("g" "Refresh" dictionary-overlay-refresh-buffer :transient nil)
   ;;  ("R" "Render buffer" dictionary-overlay-render-buffer :transient nil)
   ;;  ("k" "Known Word" dictionary-overlay-mark-word-known :transient nil)
   ;;  ("K" "Unknow Word" dictionary-overlay-mark-word-unknown :transient nil)]
   ])

(transient-define-prefix my/mpv-menu ()
  "References"
  [["Controls"
    ("[" "Speed decrease" mpv-speed-decrease :transient nil)
    ("]" "Speed increase" mpv-speed-increase :transient nil)
    ("<f8>" "Quit&Save" my/mpv-quit-with-save :transient nil)]
   ["Toggle"
    ("f" "Fullscreen" my/mpv-toggle-fullscreen :transient nil)
    ("o" "Progress" my/mpv-toggle-progress :transient nil)
    ("v" "video" mpv-toggle-video :transient nil)]])

(keymap-set global-map "s-f" my/file-prefix-map)
(keymap-set global-map "s-/" my/sort-note-prefix-map)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "s-. m") #'plantuml-org-to-mindmap-open)
  (define-key org-mode-map (kbd "s-. w") #'plantuml-org-to-wbs-open)
  (define-key org-mode-map (kbd "s-. d") #'org-export-docx))

;; transient
(global-set-key (kbd "s-b") #'my/bibtex-menu)
(global-set-key (kbd "s-l") #'my/links-menu)
(global-set-key (kbd "s-m") #'mu4e)

;; Notes
;; (keymap-set global-map "s-n n" my/new-note-prefix-map)
(global-set-key (kbd "s-n a") #'ibooks-annot/extract-annotations-to-note)
(global-set-key (kbd "s-n n") #'denote)
;; (global-set-key (kbd "s-n c") #'denote-fz-insert-dwim)
(global-set-key (kbd "s-n c") #'denote-sequence-new-child-of-current)
;; (global-set-key (kbd "s-n s") #'denote-fz-insert-at-level-dwim)
(global-set-key (kbd "s-n s") #'denote-sequence-new-sibling-of-current)
(global-set-key (kbd "s-n b") #'my/new-blog)
(global-set-key (kbd "s-n e") #'denote-org-extras-extract-org-subtree)
(global-set-key (kbd "s-n m") #'my/new-meeting)
(global-set-key (kbd "s-n l") #'my/literature-save)
(global-set-key (kbd "s-n r") #'citar-create-note)
(global-set-key (kbd "s-n k") #'denote-rename-file-keywords)
(global-set-key (kbd "s-n t") #'denote-rename-file-title)
(global-set-key (kbd "s-n o") #'my/ocr)

(global-set-key (kbd "C-t") #'my/dict-menu)

;; Specific Application
(global-set-key (kbd "C-c g") #'my/bean-generate)
(global-set-key (kbd "C-c f") #'my/beancount-fava)
(global-set-key (kbd "C-c t") #'telega)
;; (global-set-key (kbd "s-a p") #'password-store-menu)
(global-set-key (kbd "C-c e") #'elfeed)

;; Search related
(global-set-key (kbd "s-s r") #'rg)
(global-set-key (kbd "s-s g") #'my/search-google)
(global-set-key (kbd "s-s w") #'my/search-wikipedia_en)
(global-set-key (kbd "s-s z") #'my/search-zhihu)
(global-set-key (kbd "s-s n") #'denote-search)
(global-set-key (kbd "s-s m") #'my/search-doubanmovie)
(global-set-key (kbd "s-s b") #'my/search-doubanbook)
(global-set-key (kbd "s-s y") #'my/search-youtube)
(global-set-key (kbd "s-s s") #'my/search-scholar)
(global-set-key (kbd "s-s S") #'my/search-semanticscholar)

;; (global-set-key (kbd "<f12>") #'my/agenda-menu)
(global-set-key (kbd "C-<f8>") #'my/mpv-menu)

(global-set-key (kbd "M-g ,") #'switch-to-minibuffer)

(global-set-key (kbd "C-z") #'repeat)

;; 下面的方法可以使用弹窗。
;; 目前没有想好有什么合适的使用场景。
;; 不一定需要绑定全局，可以使用 local map。
;; (defun freedom-context-menu (event)
;;   "鼠标右键菜单"
;;   (interactive "e")
;;   (popup-menu
;;    '("Freedom Menu"
;; 	 ["Delete window"        (delete-window)]
;; 	 ["Delete other window"  (delete-other-windows)]
;;      ["Spilt window right"   (split-window-right)]
;;      ["Spilt winodw below"   (split-window-below)]
;;      ["Agenda"       (org-gtd-engage)]
;;      )))
;; (global-set-key [mouse-3] 'freedom-context-menu)

;;
;; (use-package casual-calc
;;   :bind (:map calc-mode-map
;;               ("C-o" . casual-calc-tmenu)
;;               :map calc-alg-map
;;               ("C-o" . casual-calc-tmenu))
;;   :after (calc))

(provide 'init-keys)
;;; init-keys.el ends here
