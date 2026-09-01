;; -*- lexical-binding: t; -*-

;; 常用快捷键绑定
;; M-s prefix meas search something
;; M-g prefix meas go to somewhere
;; v and V 快速访问文件
;; C-x b and C-x C-b switch buffer
;; gs quick jump to scratch-buffer

;; Use C-x ] or C-x [ quick jump to next/previous page


;; 设置 which-key 显示的快捷键名称，比显示 +prefix 更明确
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

(defvar-keymap my/file-prefix-map
  :doc "Prefix map for file."
  "w" #'find-file-other-window
  ;; "j" #'find-file-other-window-no-jump
  "p" #'find-file-at-point
  "t" #'find-file-other-tab
  )

(defvar-keymap my/buffer-prefix-map
  :doc "Prefix map for buffer."
  "m" #'switch-to-message
  "s" #'scratch-buffer
  "k" #'kill-buffer
  "p" #'previous-buffer
  "n" #'next-buffer
  )

(defvar-keymap my/window-prefix-map
  :doc "Keymap for windows"
  "u" #'winner-undo
  "r" #'winner-redo
  "d" #'dired-sidebar-toggle-sidebar
  )
(keymap-set global-map "M-o" my/window-prefix-map)


;; Keybindings with transient
(unless (featurep 'transient)
  (require 'transient))

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
(global-set-key (kbd "<f12>") #'my/agenda-menu)

;; In org-mode, use C-c C-o org-open-at-point to open link.
;; Or, when cursor under a link, use embark-act to open it
;; embark-act also work with citar
(transient-define-prefix my/note-menu ()
  "Note"
  [["New Note"
    ("n" "Find or Create" consult-notes :transient nil)
    ("j" "New Journal" denote-journal-new-or-existing-entry :transient nil)
    ("s" "New Signature" denote-signature :transient nil)
    ]
   ["Meta Rename"
    ("k" "Keywords" denote-rename-file-keywords :transient nil)
    ("r" "Note" denote-rename-file-using-front-matter :transient nil)
    ("S" "Signature" denote-rename-file-signature :transient nil)
    ("t" "Title" denote-rename-file-title :transient nil)
   ]
  ["Denote Link"
   ("c" "Contents" denote-link-to-file-with-contents :transient nil)
   ("C" "All Contents" denote-link-to-all-files-with-contents :transient nil)
   ("%" "regexp" denote-link-to-all-files-with-regexp :transient nil)
   ("l" "Link" denote-link-or-create :transient nil)
   ("L" "Delete Link" jf/org-link-remove-link :transient nil)
   ]
  ["References"
   ("C-n" "Create" citar-create-note :transient nil)
   ("e" "Open entry" citar-open-entry :transient nil)
   ("f" "Open file" citar-open-files :transient nil)
   ("F" "Open note" citar-open-note :transient nil)
   ]])


;; 如果不重新绑定，hungry-delete 不工作
(with-eval-after-load 'viper
  (define-key viper-insert-global-user-map [backspace]
              #'delete-backward-char)
  (define-key viper-insert-global-user-map (kbd "C-h")
              #'delete-backward-char)
  (define-key viper-insert-global-user-map (kbd "DEL")
              #'delete-backward-char)
  (define-key viper-insert-global-user-map [?\C-?]
              #'delete-backward-char))


;; Bind keys under SPC
;; 高频使用集合命令
(with-eval-after-load 'viper
  (defvar my/viper-leader-map (make-sparse-keymap))
  (define-key viper-vi-global-user-map (kbd "SPC") my/viper-leader-map)
  (define-key viper-insert-global-user-map "\C-\\" 'toggle-input-method)
  (define-key my/viper-leader-map (kbd "SPC") #'set-mark-command)
  (define-key my/viper-leader-map (kbd "f") my/file-prefix-map)
  (define-key my/viper-leader-map (kbd "b") my/buffer-prefix-map)
  (define-key my/viper-leader-map (kbd "n") #'my/note-menu)
  )


;; Bind keys under g prefix
;; 高频使用的单一命令
(with-eval-after-load 'viper
  (define-key viper-vi-global-user-map (kbd "gg") #'beginning-of-buffer)
  (define-key viper-vi-global-user-map (kbd "gd") #'xref-find-definitions)
  (define-key viper-vi-global-user-map (kbd "gr") #'recentf-open-files)
  (define-key viper-vi-global-user-map (kbd "gs") #'scratch-buffer)
  (define-key viper-vi-global-user-map (kbd "gh") #'windmove-left)
  (define-key viper-vi-global-user-map (kbd "gj") #'windmove-down)
  (define-key viper-vi-global-user-map (kbd "gk") #'windmove-up)
  (define-key viper-vi-global-user-map (kbd "gl") #'windmove-right)
  (define-key viper-vi-global-user-map (kbd "u") #'undo)
  (define-key viper-vi-global-user-map (kbd "U") #'vundo)
  )

(provide 'init-keys)
