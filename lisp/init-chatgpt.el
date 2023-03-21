(use-package gptel
  :general (my/space-leader-def
             "mc" '(gptel :wk "ChatGPT"))
  :config
  (setq gptel-use-curl nil)
  (add-to-list 'display-buffer-alist '("^\\*ChatGPT\\*"
                                       (display-buffer-pop-up-frame)
                                       (window-parameters
                                        (mode-line-format . none)))))

(provide 'init-chatgpt)
;;; init-chatgpt.el ends here.
