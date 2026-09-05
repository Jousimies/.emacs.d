;; -*- lexical-binding: t; -*-
(global-set-key [remap list-buffers] #'ibuffer)
(add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)
(with-eval-after-load 'ibuffer
  (setq ibuffer-expert t
        ibuffer-show-empty-filter-groups nil
        ibuffer-default-sorting-mode 'major-mode))

;; bufferlo
(add-hook 'on-first-buffer-hook #'bufferlo-mode)
(global-set-key [remap switch-to-buffer] #'bufferlo-switch-to-buffer)

;; helpful
(global-set-key [remap describe-function] #'helpful-callable)
(global-set-key [remap describe-variable] #'helpful-variable)
(global-set-key [remap describe-key] #'helpful-key)
(add-to-list 'display-buffer-alist
               '("\\*helpful"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.5)
                 (window-parameters
                  (mode-line-format . none))))
;; elisp-demos in helpful
(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

;; popper
(add-hook 'on-first-buffer-hook #'popper-mode)
(global-set-key (kbd "C-`") #'popper-toggle)
(with-eval-after-load 'popper
  (define-key popper-mode-map (kbd "M-<tab>") #'popper-cycle)
  (define-key popper-mode-map (kbd "M-`") #'popper-toggle-type)

  (setq popper-mode-line '(:eval (propertize "POP" 'face `(:inverse-video t))))

  ;; Enable indicator in minibuffer
  (defun my/popper--fit-window-height (win)
    "Determine the height of popup window WIN by fitting it to the buffer's content."
    (fit-window-to-buffer
     win
     (floor (frame-height) 2)
     (floor (frame-height) 3)))
  (setq popper-window-height #'my/popper--fit-window-height)

  ;; HACK: close popper with `C-g'
  (defun +popper-close-window-hack (&rest _)
    "Close popper window via `C-g'."
    (when (and (called-interactively-p 'interactive)
               (not (region-active-p))
               popper-open-popup-alist)
      (let ((window (caar popper-open-popup-alist)))
        (when (window-live-p window)
          (delete-window window)))))
  (advice-add #'keyboard-quit :before #'+popper-close-window-hack))

(with-eval-after-load 'popper
  (dolist (group '((standard "\\*Messages\\*" "Output\\*$" "\\*Pp Eval Output\\*$"
                             "\\*Compile-Log\\*" "\\*Completions\\*" "\\*Warnings\\*"
                             "\\*Async Shell Command\\*" "\\*Apropos\\*" "\\*Backtrace\\*"
                             "\\*Embark Actions\\*" "\\*Finder\\*" "\\*Kill Ring\\*"
                             "\\*Go-Translate\\*")
                   (modes bookmark-bmenu-mode comint-mode compilation-mode
                          help-mode helpful-mode tabulated-list-mode Buffer-menu-mode)
                   (docs gnus-article-mode devdocs-mode grep-mode occur-mode
                         rg-mode ag-mode pt-mode youdao-dictionary-mode
                         osx-dictionary-mode fanyi-mode)
                   (process "^\\*Process List\\*" process-menu-mode list-environment-mode
                            cargo-process-mode "^\\*EKG Capture" "^\\*Ibuffer\\*"
                            ibuffer-mode "^\\*eshell.*\\*.*$" eshell-mode
                            "^\\*shell.*\\*.*$" shell-mode "^\\*terminal.*\\*.*$"
                            term-mode "^\\*vterm.*\\*.*$" vterm-mode
                            "^\\*eldoc.*\\*.*$" eldoc-mode)
                   (dev "\\*Flycheck errors\\*$" " \\*Flycheck checker\\*$"
			"\\*Paradox Report\\*$" "\\*package update results\\*$"
			"\\*Package-Lint\\*$" "\\*[Wo]*Man.*\\*$" "\\*ert\\*$"
			overseer-buffer-mode "\\*gud-debug\\*$" "\\*lsp-help\\*$"
			"\\*lsp session\\*$" "\\*quickrun\\*$" "\\*tldr\\*$"
			"\\*vc-.*\\*$" "^\\*elfeed-entry\\*$" "^\\*macro expansion\\**")
                   (org "\\*TeX Help\\*" "^\\*denote-backlinks to "
			"\\*Agenda Commands\\*" "\\*Org Select\\*"
			"\\*Org Note\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*")))
    (dolist (buf (cdr group))
      (add-to-list 'popper-reference-buffers buf))))

;; (with-eval-after-load 'ibuffer
;;   (define-key ibuffer-mode-map (kbd "RET") #'+ibuffer-visit-buffer-in-popper))

(provide 'init-buffer)
