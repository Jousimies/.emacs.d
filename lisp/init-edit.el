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

(global-set-key (kbd "C-=") #'expreg-expand)
(global-set-key (kbd "C--") #'expreg-contract)

(global-set-key (kbd "C-c p") #'cape-prefix-map)

(with-eval-after-load 'minibuffer
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;;;###autoload
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

;;;####autoload
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

;;;###autoload
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

(add-hook 'post-select-region-hook #'selected-minor-mode)
(with-eval-after-load 'selected
  (define-key selected-keymap (kbd "q") #'selected-off)
  (define-key selected-keymap (kbd "x") #'kill-region)
  (define-key selected-keymap (kbd "w") #'count-words-region)
  (define-key selected-keymap (kbd "i") #'surround-insert)
  (define-key selected-keymap (kbd "c") #'surround-change)
  (define-key selected-keymap (kbd "d") #'surround-delete)
  (define-key selected-keymap (kbd "s") #'my/org-insert-emphasis-with-zws)
  (define-key selected-keymap (kbd "S") #'my/org-element-unwrap-emphasis)
  (define-key selected-keymap (kbd "m") #'apply-macro-to-region-lines)
  (define-key selected-keymap (kbd "\\") #'indent-region)
  (define-key selected-keymap (kbd ";") #'comment-dwim)
  (define-key selected-keymap (kbd "k") #'my/selected-wrap-textcolo))

;;;###autoload
(defun my/embark-symbol-overlay-toggle ()
      "如果当前符号未高亮，则高亮它；
如果当前符号已经处于高亮状态，则清除缓冲区内所有高亮。"
      (interactive)
      (if (get-char-property (point) 'symbol-overlay)
	  (progn
            (symbol-overlay-remove-all)
            (message "Cleared all highlights."))
	(symbol-overlay-put)))
(add-hook 'prog-mode-hook #'symbol-overlay-mode)
(add-hook 'html-mode-hook #'symbol-overlay-mode)
(advice-add 'embark-toggle-highlight :override #'my/embark-symbol-overlay-toggle)


(add-hook 'on-first-file-hook #'undo-fu-session-global-mode)

;;;###autoload
(defun my/undo-fu-session--make-file-name (filename)
    "Take the path FILENAME and return a name base on this."
    (concat
     (file-name-concat undo-fu-session-directory
                       (md5 (convert-standard-filename (expand-file-name filename))))
     (undo-fu-session--file-name-ext)))

(with-eval-after-load 'undo-fu-session
  (setq undo-fu-session-directory (expand-file-name "undo-fu-session/" cache-directory))
  (advice-add 'undo-fu-session--make-file-name :override #'my/undo-fu-session--make-file-name))

(with-eval-after-load 'vundo
  (setq vundo-glyph-alist vundo-unicode-symbols))

(add-hook 'on-first-input-hook #'global-hungry-delete-mode)
(with-eval-after-load 'hungry-delete
  (setq hungry-delete-chars-to-skip " \t\n\r\f\v"))

;; IME
;; 如果 Emacs 启动报 liberime-load 相关错误，将 .emacs.d/module/liberime 路径下的 dll 文件复制到 Emacs 的安装目录
(with-eval-after-load 'liberime
  (setq liberime-verbose nil))
(setq liberime-module-file
      (cond
       (sys/win32p (expand-file-name "module/liberime/liberime-core.dll" user-emacs-directory))
       (sys/macp   (expand-file-name "module/liberime-core.dylib" user-emacs-directory))))
(setq liberime-user-data-dir
      (cond
       (sys/win32p "~/AppData/Roaming/Rime")
       (sys/macp   "~/Library/Rime/")))

(with-eval-after-load 'rimel
  (custom-set-faces
   '(rimel-candidate-label-face ((t (:inherit font-lock-comment-face :height 0.85))))
   '(rimel-page-indicator-face ((t (:inherit font-lock-comment-face :height 0.85))))
   '(rimel-highlight-face ((t (:inherit hl-line)))))

  (setq default-input-method "rimel")
  (setq rimel-inline-preedit t)
  (setq rimel-candidate-show-preedit nil)
  (setq rimel-candidate-label-format "%d ")
  (setq rimel-page-indicator-format "%d%s")
  (setq rimel-disable-predicates '(rimel-predicate-prog-in-code-p
                                   rimel-predicate-after-alphabet-char-p
                                   rimel-predicate-current-uppercase-letter-p
                                   rimel-predicate-org-in-src-block-p
                                   rimel-predicate-org-latex-mode-p
                                   rimel-predicate-tex-math-or-command-p)))

(add-hook 'on-first-input-hook
          (lambda ()
	      (unless (assoc "rimel" input-method-alist)
                (register-input-method
                 "rimel" "Chinese" #'rimel-activate
                 (if (char-displayable-p 12563) (char-to-string 12563) "中")
                 "Rimel - Rime input method via liberime"))))

(with-eval-after-load 'rimel
  (with-eval-after-load 'posframe
    (setq rimel-show-candidate 'posframe)
    (setq rimel-posframe-style 'horizontal)))

(add-hook 'on-first-input-hook #'liberime-regexp-mode)
(add-hook 'on-first-input-hook #'liberime-regexp-avy-mode)

(with-eval-after-load 'liberime-regexp
  (setq liberime-regexp-auto-build nil)
  (setq liberime-regexp-segment-mode nil)
  (global-set-key [remap goto-char] #'liberime-regexp-avy-goto-char-timer))

(add-hook 'on-first-input-hook #'sis-global-inline-mode)
(add-hook 'on-first-input-hook #'sis-global-context-mode)
(add-hook 'on-first-input-hook #'sis-global-cursor-color-mode)
(add-hook 'viper-vi-state-hook #'sis-set-english)

(with-eval-after-load 'sis
  (sis-ism-lazyman-config nil "rimel" 'native)

  (add-to-list 'sis-context-hooks 'viper-insert-state-hook)

  ;; Defvars and helper functions
  (defvar-local +sis-inline-english-last-space-pos nil
    "The last space position in inline mode.")

  (defun +sis-line-set-last-space-pos ()
    (when (eq (char-before) ?\s)
      (setq +sis-inline-english-last-space-pos (point))))
  (add-hook 'sis-inline-english-activated-hook #'+sis-line-set-last-space-pos)

  (add-hook 'sis-inline-mode-hook #'+sis-inline-add-post-self-insert-hook)

  (defun +sis-inline-add-post-self-insert-hook ()
    (add-hook 'post-self-insert-hook
              (defun +sis-inline-remove-redundant-space ()
                (when (and (eq +sis-inline-english-last-space-pos (1- (point)))
                           (looking-back (concat " [" +sis-chinese-puncs "]")))
                  (save-excursion
                    (backward-char 2)
                    (delete-char 1)
                    (setq-local +sis-inline-english-last-space-pos nil))))
              nil
              'local))

  ;; Chinese punc adjustment for inline mode
  (defconst +sis-chinese-puncs "，。？！；：（【「“")

  (defconst +sis-chinese-punc-chars (string-to-list +sis-chinese-puncs))

  (defun +sis-remove-head-space-after-cc-punc (_)
    (when (or (memq (char-before) +sis-chinese-punc-chars)
              (bolp))
      (delete-char 1)))
  (setq sis-inline-tighten-head-rule #'+sis-remove-head-space-after-cc-punc)

  (defun +sis-remove-tail-space-before-cc-punc (_)
    "Only delete the trailing space when followed by Chinese punctuation."
    (when (and (eq (char-before) ?\s)
               (memq (char-after) +sis-chinese-punc-chars))
      (backward-delete-char 1)))

  (setq sis-inline-tighten-tail-rule #'+sis-remove-tail-space-before-cc-punc))


(provide 'init-edit)
