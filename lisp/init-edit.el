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
  :ensure t
  :bind (("C-=" . expreg-expand)
         ("C--" . expreg-contract)))

(use-package consult-dir
  :ensure t
  :bind (([remap list-directory] . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package cape
  :ensure t
  :bind ("C-c p" . cape-prefix-map))
(add-hook 'completion-at-point-functions #'cape-dabbrev)
(add-hook 'completion-at-point-functions #'cape-file)
(add-hook 'completion-at-point-functions #'cape-elisp-block)

(use-package surround
  :ensure t
  :commands surround-delete surround-change surround-insert)

(use-package selected
  :ensure t
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
  :ensure t
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
  :ensure t
  :hook (on-first-file . undo-fu-session-global-mode)
  :custom
  (undo-fu-session-directory (expand-file-name "undo-fu-session/" cache-directory)))

(use-package vundo
  :ensure t
  :commands vundo
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))

(use-package hungry-delete
  :ensure t
  :hook (on-first-input . global-hungry-delete-mode))

;; IME

(use-package liberime
  :ensure t
  :commands liberime-load
  :custom
  (liberime-module-file "C:/Program Files/Emacs/emacs-30.2/bin/liberime-core.dll")
  (liberime-user-data-dir "~/AppData/Roaming/Rime"))

(use-package rimel
  :ensure t
  :defer t
  :custom
  (default-input-method "rimel")
  (rimel-disable-predicates
      '(rimel-predicate-prog-in-code-p
        rimel-predicate-after-alphabet-char-p
        rimel-predicate-current-uppercase-letter-p))
  :config
  (use-package posframe
    :ensure t
    :custom
    (rimel-show-candidate 'posframe)
    (rimel-posframe-style 'horizontal))
  (add-to-list 'rimel-disable-predicates 'rimel-predicate-org-in-src-block-p))

(use-package liberime-regexp
  :vc (:url "https://github.com/roife/liberime-regexp.git"
	    :rev "main")
  :bind ([remap goto-char] . liberime-regexp-avy-goto-char-timer)
  :hook ((on-first-file . liberime-regexp-mode)
	 (liberime-after-start . liberime-regexp-avy-mode)))


(provide 'init-edit)
