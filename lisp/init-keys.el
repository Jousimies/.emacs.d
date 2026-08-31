;; -*- lexical-binding: t; -*-

;; which-key
(with-eval-after-load 'which-key
  (dolist (map '(("M-s h" . "highlight")
		 ("C-x 8" . "unicode")
                 ("C-x 8 e" . "emoji")
		 ("C-c e" . "Denote Explore")
		 ("C-c f" . "Folder/Files")
		 ("SPC f" . "Folder/Files")
		 ("SPC n" . "Notes")
		 ("SPC b" . "Buffer")
		 ("C-x n" . "narrow")
		 ))
    (which-key-add-key-based-replacements (car map) (cdr map))))

(with-eval-after-load 'viper
  (define-key viper-vi-global-user-map (kbd "gg") #'beginning-of-buffer)
  (define-key viper-vi-global-user-map (kbd "gb") #'bufferlo-switch-to-buffer)
  (define-key viper-vi-global-user-map (kbd "gc") #'goto-char)
  (define-key viper-vi-global-user-map (kbd "gd") #'xref-find-definitions)
  (define-key viper-vi-global-user-map (kbd "gr") #'recentf-open-files)
  (define-key viper-vi-global-user-map (kbd "gn") #'my/note-menu)
  (define-key viper-vi-global-user-map (kbd "gh") #'windmove-left)
  (define-key viper-vi-global-user-map (kbd "gj") #'windmove-down)
  (define-key viper-vi-global-user-map (kbd "gk") #'windmove-up)
  (define-key viper-vi-global-user-map (kbd "gl") #'windmove-right)
  (define-key viper-vi-global-user-map (kbd "u") #'undo)
  (define-key viper-vi-global-user-map (kbd "U") #'vundo)

  (define-key viper-insert-global-user-map [backspace]
              #'delete-backward-char)
  (define-key viper-insert-global-user-map (kbd "C-h")
              #'delete-backward-char)
  (define-key viper-insert-global-user-map (kbd "DEL")
              #'delete-backward-char)
  (define-key viper-insert-global-user-map [?\C-?]
              #'delete-backward-char))

(defvar-keymap my/file-prefix-map
  :doc "Prefix map for file."
  "f" #'find-file
  "w" #'find-file-other-window
  "j" #'find-file-other-window-no-jump
  "p" #'find-file-at-point
  "t" #'find-file-other-tab
  "r" #'consult-recent-file
  )

(defvar-keymap my/buffer-prefix-map
  :doc "Prefix map for buffer."
  "b" #'bufferlo-switch-to-buffer
  "i" #'consult-buffer
  "I" #'ibuffer
  "m" #'switch-to-message
  "s" #'scratch-buffer
  "k" #'kill-buffer
  )

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
  ;; "d" #'dired-sidebar-toggle-sidebar
  )
(require 'transient)
(transient-define-prefix my/agenda-menu ()
  "GTD"
  [["Agenda"
	("a" "Agenda" org-agenda :transient nil)]
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
	("f" "Area of Focus" org-gtd-review-area-of-focus :transient t)]
   ])

(transient-define-prefix my/note-menu ()
  "Note"
  [["New Note"
	("n" "Find" consult-notes :transient nil)
	("N" "Denote" denote :transient nil)
	("j" "Journal" denote-journal-new-or-existing-entry :transient nil)
	]
   ["Denote Meta"
	("r" "Rename Note" denote-rename-file-using-front-matter :transient nil)
	("k" "Add Keyword" denote-keywords-add :transient nil)
	("K" "Remove Keyword" denote-keywords-remove :transient nil)]
   ])


(global-set-key (kbd "<f12>") #'my/agenda-menu)

(with-eval-after-load 'viper
  (defvar my/viper-leader-map (make-sparse-keymap))
  (define-key viper-vi-global-user-map (kbd "SPC") my/viper-leader-map)
  (define-key viper-insert-global-user-map "\C-\\" 'toggle-input-method)
  (define-key my/viper-leader-map (kbd "SPC") #'execute-extended-command)
  (define-key my/viper-leader-map (kbd "f") my/file-prefix-map)
  (define-key my/viper-leader-map (kbd "b") my/buffer-prefix-map)
  (define-key my/viper-leader-map (kbd "n") #'my/note-menu)
  )

(keymap-set global-map "M-o" my/window-prefix-map)


(provide 'init-keys)
