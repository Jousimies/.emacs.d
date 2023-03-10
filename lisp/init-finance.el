(use-package beancount
  :mode (".bean" . beancount-mode)
  :hook ((beancount-mode . (lambda ()
                             (setq-local electric-indent-chars nil)))
         (beancount-mode . outline-minor-mode))
  :config
  (evil-define-key 'normal 'beancount-mode-map
    "zf" 'beancount-fava)

  ;; insert whole transaction instead of only insert date.
  (defun my/beancount-insert-transaction (&optional days)
    "Start a new timestamped directive with date shifted by DAYS from today."
    (interactive "P")
    (unless (bolp) (newline))
    (insert (beancount--shift-current-date days) " * \"\" \"\"")
    (evil-backward-char 3)
    (evil-insert 0))

  (advice-add 'beancount-insert-date :override 'my/beancount-insert-transaction)

  ;; Auto open browser after beancount-fava started.
  (defun my/browser-beancount-fava ()
    (if beancount--fava-process
        (browse-url "http://127.0.0.1:5000")))

  (advice-add 'beancount-fava :after 'my/browser-beancount-fava)

  ;; auto align transaction before save file.
  (defun my/beancount-align-transaction ()
    "Align visible region in current buffer."
    (save-excursion
      (indent-region (window-start) (window-end))))

  (add-hook 'before-save-hook (lambda ()
                                (if (eq major-mode 'beancount-mode)
                                    (my/beancount-align-transaction))))

  ;; If cursor in "", activate input method rime.
  (defun my/beancount-activate-input-method ()
    (when (eq major-mode 'beancount-mode)
      (if (not (bounds-of-thing-at-point 'whitespace))
          (if (bounds-of-thing-at-point 'string)
              (activate-input-method "rime")))))

  (add-hook 'evil-insert-state-entry-hook #'my/beancount-activate-input-method))

(use-package achive
  :commands achive
  :config
  (setq achive-language 'zh)
  (setq achive-cache-path (expand-file-name "chche/.achive" user-emacs-directory)))

(my/space-leader-def
  "ms" '(achive :wk "Share"))

(add-to-list 'display-buffer-alist '("\\*A Chive\\*"
                                     (display-buffer-in-new-tab)
                                     (tab-name . "Share")
                                     (tab-group . "Misc")))
(with-eval-after-load 'evil-collection
  (evil-collection-define-key 'normal 'achive-visual-mode-map
    "q" 'quit-window))

(provide 'init-finance)
;;; init-finance.el ends here.
