;; -*- lexical-binding: t; -*-

;; 常用快捷键绑定
;; M-s prefix meas search something
;; M-g prefix meas go to somewhere
;; v and V 快速访问文件
;; C-x b and C-x C-b switch buffer
;; gs quick jump to scratch-buffer

;; Use C-x ] or C-x [ quick jump to next/previous page

(global-set-key (kbd "M-g m") #'switch-to-message)
(global-set-key (kbd "M-g s") #'scratch-buffer)
(global-set-key (kbd "C-x 1") #'toggle-delete-other-windows)

(global-set-key (kbd "C-x p R") #'my/open-project-readme)
(global-set-key (kbd "C-x p n") #'generate-folder-tree)


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
  "h" #'windmove-left
  "j" #'windmove-down
  "k" #'windmove-up
  "l" #'windmove-right
  "d" #'dired-sidebar-toggle-sidebar
  )
(keymap-set global-map "M-o" my/window-prefix-map)


;; Keybindings with transient.  Loading transient costs noticeable startup time,
;; so define these menus on first use.
(defun my/ensure-key-transients ()
  "Define transient menus used by `my/agenda-menu' and `my/note-menu'."
  (unless (fboundp 'my/agenda-menu--transient)
    (require 'transient)
    (eval
     '(transient-define-prefix my/agenda-menu--transient ()
        "GTD"
        [["Agenda"
          ("a" "Agenda" org-agenda :transient nil)]
         ["Process & Engage"
          ("x" "Process Inbox" org-gtd-process-inbox :transient nil)
          ;; ("@" "By Context" org-gtd-engage-grouped-by-context :transient nil)
          ("<f12>" "Engage" org-gtd-engage :transient nil)]
         ;; ["Clarify"
         ;;  ("c" "Item" org-gtd-clarify-item :transient nil)
         ;;  ("C" "Item: agenda" org-gtd-clarify-agenda-item :transient nil)]
         ;; ["Review"
         ;;  ("o" "Missed Appointments" org-gtd-oops :transient t)
         ;;  ("m" "Missed Items" org-gtd-review-missed-items :transient t)
         ;;  ("f" "Area of Focus" org-gtd-review-area-of-focus :transient t)]
	 ]))
    (eval
     '(transient-define-prefix my/note-menu--transient ()
        "Note"
	[[("n" "Find or Create" consult-notes :transient nil)]]
        [["New Note"
          ("sc" "Child" denote-sequence-new-child-of-current :transient nil)
          ("sp" "Parents" denote-sequence-new-parent :transient nil)
          ("sb" "Sibling" denote-sequence-new-sibling-of-current :transient nil)
          ("j" "Journal" denote-journal-new-or-existing-entry :transient nil)
          ("b" "Blogs" my/blog-new-post :transient nil)]
         ["Meta Rename"
          ("k" "Keywords" denote-rename-file-keywords :transient nil)
          ("r" "Note" denote-rename-file-using-front-matter :transient nil)
          ("S" "Signature" denote-rename-file-signature :transient nil)
          ("t" "Title" denote-rename-file-title :transient nil)]
         ["Denote Link"
          ("c" "Contents" denote-link-to-file-with-contents :transient nil)
          ("C" "All Contents" denote-link-to-all-files-with-contents :transient nil)
          ("%" "regexp" denote-link-to-all-files-with-regexp :transient nil)
          ("l" "Link" denote-link-or-create :transient nil)
          ("L" "Delete Link" jf/org-link-remove-link :transient nil)]
         ["References"
          ("o" "Citar Open" citar-open :transient nil)
          ("e" "Open entry" citar-open-entry :transient nil)
          ("f" "Open file" citar-open-files :transient nil)
          ("O" "Open note" citar-open-note :transient nil)]
         ["Misc"
          ("P" "Blog Push" my/blog-sync :transient nil)]]))))

(defun my/agenda-menu ()
  "Open GTD transient menu."
  (interactive)
  (my/ensure-key-transients)
  (call-interactively #'my/agenda-menu--transient))
(global-set-key (kbd "<f12>") #'my/agenda-menu)

(defun my/note-menu ()
  "Open note transient menu."
  (interactive)
  (my/ensure-key-transients)
  (call-interactively #'my/note-menu--transient))

;; Viper 的 insert map 优先级高于 `electric-pair-mode-map'。
;; 直接绑定到 `delete-backward-char' 会绕过 Emacs 原生的成对删除；
;; 但直接绑定到 `electric-pair-delete-pair' 在普通位置会报 end-of-buffer。
(defun my/electric-pair-adjacent-p ()
  "Return non-nil when point is between an adjacent pair like (|) or \"|\"."
  (let ((prev (char-before))
        (next (char-after)))
    (and prev next
         (or (eq (cdr (assq prev electric-pair-pairs)) next)
             (eq (cdr (assq prev electric-pair-text-pairs)) next)
             (and (eq (char-syntax prev) ?\()
                  (eq (char-syntax next) ?\))
                  (ignore-errors
                    (= (scan-sexps (1- (point)) 1) (1+ (point)))))))))

(defun my/viper-electric-backward-delete (&optional arg killp)
  "Delete backward in Viper insert state, preserving electric pair deletion."
  (interactive "p\nP")
  (if (and (bound-and-true-p electric-pair-mode)
           (my/electric-pair-adjacent-p))
      (electric-pair-delete-pair arg killp)
    (backward-delete-char-untabify arg killp)))

(with-eval-after-load 'viper
  (require 'elec-pair)
  (define-key viper-insert-global-user-map [backspace]
              #'my/viper-electric-backward-delete)
  (define-key viper-insert-global-user-map (kbd "C-h")
              #'my/viper-electric-backward-delete)
  (define-key viper-insert-global-user-map (kbd "DEL")
              #'my/viper-electric-backward-delete)
  (define-key viper-insert-global-user-map [?\C-?]
              #'my/viper-electric-backward-delete))


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
;; gh 等命令在某些 buffer 中会不可用，还是使用 M-o 进行转换。
(with-eval-after-load 'viper
  (define-key viper-vi-global-user-map (kbd "gg") #'beginning-of-buffer)
  (define-key viper-vi-global-user-map (kbd "gd") #'xref-find-definitions)
  (define-key viper-vi-global-user-map (kbd "gr") #'recentf-open-files)
  (define-key viper-vi-global-user-map (kbd "gs") #'my/org-insert-emphasis-with-zws)
  (define-key viper-vi-global-user-map (kbd "gS") #'my/org-element-unwrap-emphasis)
  (define-key viper-vi-global-user-map (kbd "u") #'undo)
  (define-key viper-vi-global-user-map (kbd "U") #'vundo)
  )

(provide 'init-keys)
