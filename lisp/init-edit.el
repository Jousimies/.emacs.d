;; -*- lexical-binding: t; -*-

(define-key global-map [remap dabbrev-expand] #'hippie-expand)
(with-eval-after-load 'hippie-exp
  (setq hippie-expand-try-functions-list '(try-complete-file-name-partially
					   try-complete-file-name
					   try-expand-all-abbrevs
					   try-expand-dabbrev
					   try-expand-dabbrev-all-buffers
					   try-expand-dabbrev-from-kill
					   try-complete-lisp-symbol-partially
					   try-complete-lisp-symbol)))

(use-package expreg
  :bind (("C-=" . expreg-expand)
         ("C--" . expreg-contract)))

(use-package consult-dir
  :bind (([remap list-directory] . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package cape
  :bind ("C-c p" . cape-prefix-map))
(add-hook 'completion-at-point-functions #'cape-dabbrev)
(add-hook 'completion-at-point-functions #'cape-file)
(add-hook 'completion-at-point-functions #'cape-elisp-block)

(use-package surround
  :commands surround-delete surround-change surround-insert)

(use-package selected
  :preface
  (defun my/selected-wrap-textcolor (color)
    "用 \textcolor{COLOR}{region} 包裹选中的文字。"
    (interactive "sEnter color (default red): ")
    (let ((c (if (string-empty-p color) "red" color))
          (beg (region-beginning))
          (end (region-end)))
      (save-excursion
	(goto-char end)
	(insert "}")
	(goto-char beg)
	(insert (format "\\textcolor{%s}{" c)))))

  (defun my/org-insert-emphasis-with-zws (marker)
    "在标记符两侧自动插入零宽空格 (U+200B) 并包裹内容。"
    (interactive "sEnter marker (e.g. *, ~, =. default =): ")
    (let* ((c (if (string-empty-p marker) "*" marker))
	   (zws "\u200b")
           (has-region (use-region-p))
           (beg (if has-region (region-beginning) (point)))
           (end (if has-region (region-end) (point))))
      (goto-char end)
      (insert c zws)
      (goto-char beg)
      (insert zws c)
      (if has-region
          (goto-char (+ end 4))
	(forward-char 2))))

  (defun my/org-element-unwrap-emphasis ()
    "参照 jf/org-link-remove-link 的逻辑，精准删除标记符及两侧的零宽空格。"
    (interactive)
    (let ((elem (org-element-context))
          (zws ?\u200b))
      (when (memq (car elem) '(bold italic code verbatim strike-through underline))
	(let* ((begin (org-element-property :begin elem))
               (end (org-element-property :end elem))
               (marker-char (buffer-substring-no-properties begin (1+ begin)))
               (actual-beg begin)
               (actual-end end)
               content)
          (setq content
		(if (org-element-property :contents-begin elem)
                    (buffer-substring-no-properties
                     (org-element-property :contents-begin elem)
                     (org-element-property :contents-end elem))
                  (buffer-substring-no-properties (+ begin 1) (- end 1))))

          (when (eq (char-before begin) zws)
            (setq actual-beg (1- begin)))

          (if (eq (char-after end) zws)
              (setq actual-end (1+ end))
            (when (eq (char-before end) zws)
              (setq actual-end end)))

          (delete-region actual-beg actual-end)
          (insert content)
          (message "已清理标记: %s" marker-char)))))
  :hook (post-select-region . selected-minor-mode)
  :bind (:map selected-keymap
              ("q" . selected-off)
              ("x" . kill-region)
              ("w" . count-words-region)
              ("i" . surround-insert)
              ("c" . surround-change)
	      ("d" . surround-delete)
              ("s" . my/org-insert-emphasis-with-zws)
	      ("S" . my/org-element-unwrap-emphasis)
              ("m" . apply-macro-to-region-lines)
              ("\\" . indent-region)
              (";" . comment-dwim)
	      ("k" . my/selected-wrap-textcolor)))

(use-package symbol-overlay
  :hook ((prog-mode . symbol-overlay-mode)
         (html-mode . symbol-overlay-mode)))

(with-eval-after-load 'embark
  (unless (featurep 'symbol-overlay)
    (require 'symbol-overlay))
  (defun my/embark-symbol-overlay-toggle ()
    "如果当前符号未高亮，则高亮它；
如果当前符号已经处于高亮状态，则清除缓冲区内所有高亮。"
    (interactive)
    (if (get-char-property (point) 'symbol-overlay)
	(progn
          (symbol-overlay-remove-all)
          (message "Cleared all highlights."))
      (symbol-overlay-put)))
  (advice-add 'embark-toggle-highlight :override #'my/embark-symbol-overlay-toggle))

(use-package undo-fu-session
  :hook (on-first-file . undo-fu-session-global-mode)
  :custom
  (undo-fu-session-directory (expand-file-name "undo-fu-session/" cache-directory))
  :config
  (defun my/undo-fu-session--make-file-name (filename)
    "Take the path FILENAME and return a name base on this."
    (concat
     (file-name-concat undo-fu-session-directory
                       (md5 (convert-standard-filename (expand-file-name filename))))
     (undo-fu-session--file-name-ext)))
  (advice-add 'undo-fu-session--make-file-name :override #'my/undo-fu-session--make-file-name))

(use-package vundo
  :commands vundo
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))

(use-package hungry-delete
  :hook (on-first-input . global-hungry-delete-mode))

;; IME

(use-package liberime
  :commands liberime-load
  :custom
  (liberime-module-file "C:/Program Files/Emacs/emacs-30.2/bin/liberime-core.dll")
  (liberime-user-data-dir "~/AppData/Roaming/Rime"))

(use-package rimel
  :defer t
  :custom-face
  (rimel-candidate-label-face ((t (:inherit font-lock-comment-face :height 0.85))))
  (rimel-page-indicator-face ((t (:inherit font-lock-comment-face :height 0.85))))
  (rimel-highlight-face ((t (:inherit hl-line))))
  :custom
  (default-input-method "rimel")
  (rimel-inline-preedit t)
  (rimel-candidate-show-preedit nil)
  (rimel-candidate-label-format "%d ")
  (rimel-page-indicator-format "%d%s")
  (rimel-disable-predicates '(rimel-predicate-prog-in-code-p
                              rimel-predicate-after-alphabet-char-p
                              rimel-predicate-current-uppercase-letter-p
                              rimel-predicate-org-in-src-block-p
                              rimel-predicate-org-latex-mode-p
                              rimel-predicate-tex-math-or-command-p))
  :config
  (use-package posframe
    :custom
    (rimel-show-candidate 'posframe)
    (rimel-posframe-style 'horizontal))
  (add-to-list 'rimel-disable-predicates 'rimel-predicate-org-in-src-block-p))

(use-package liberime-regexp
  :ensure (liberime-regexp :repo "https://github.com/roife/liberime-regexp.git" :branch "main")
  :hook ((meow-insert-enter . liberime-regexp-mode)
	 (meow-insert-enter . liberime-regexp-avy-mode))
  :bind ([remap goto-char] . liberime-regexp-avy-goto-char-timer))

(use-package sis
  :init
  (sis-ism-lazyman-config nil "rimel" 'native)
  :config
  (sis-global-inline-mode)
  (sis-global-context-mode)
  (sis-global-cursor-color-mode)
  (add-hook 'meow-insert-exit-hook #'sis-set-english)
  (add-to-list 'sis-context-hooks 'meow-insert-enter-hook)

  (defvar-local +sis-inline-english-last-space-pos nil
    "The last space position in inline mode.")

  (defun +sis-line-set-last-space-pos ()
    (when (eq (char-before) ?\s)
      (setq +sis-inline-english-last-space-pos (point))))
  (add-hook 'sis-inline-english-activated-hook #'+sis-line-set-last-space-pos)

  (add-hook 'sis-inline-mode-hook #'+sis-inline-add-post-self-insert-hook)

  (defun +sis-inline-add-post-self-insert-hook ()
    (add-hook! post-self-insert-hook :local
               (defun +sis-inline-remove-redundant-space ()
		 (when (and (eq +sis-inline-english-last-space-pos (1- (point)))
			    (looking-back (concat " [" +sis-chinese-puncs "]")))
		   (save-excursion
		     (backward-char 2)
		     (delete-char 1)
		     (setq-local +sis-inline-english-last-space-pos nil))))))
  ;; Chinese punc adjustment for inline mode
  (defconst +sis-chinese-puncs "，。？！；：（【「“")

  (defconst +sis-chinese-punc-chars (string-to-list +sis-chinese-puncs))

  (defun +sis-remove-head-space-after-cc-punc (_)
    (when (or (memq (char-before) +sis-chinese-punc-chars)
              (bolp))
      (delete-char 1)))
  (setq sis-inline-tighten-head-rule #'+sis-remove-head-space-after-cc-punc)

  (defun +sis-remove-tail-space-before-cc-punc (_)
    (when (eq (char-before) ? )
      (backward-delete-char 1)
      (when (and (eq (char-before) ? )
                 (memq (char-after) +sis-chinese-punc-chars))
        (backward-delete-char 1))))
  (setq sis-inline-tighten-tail-rule #'+sis-remove-tail-space-before-cc-punc))

(provide 'init-edit)
