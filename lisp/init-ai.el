;; -*- lexical-binding: t; -*-

(with-eval-after-load 'gptel
  (add-hook 'gptel-post-stream-hook #'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions #'gptel-end-of-response)

  (setq gptel-model 'deepseek-v4-flash)
  (setq gptel-backend (gptel-make-deepseek "DeepSeek"
                        :stream t
                        :key 'gptel-api-key)))

;; gptel-quick
(with-eval-after-load 'embark
  (keymap-set embark-general-map "?" #'gptel-quick))

(with-eval-after-load 'gptel-quick
  (setq gptel-quick-backend (gptel-make-deepseek "DeepSeek"
                              :stream t
                              :key 'gptel-api-key)
        gptel-quick-model 'deepseek-v4-flash
        gptel-quick-word-count 500
        gptel-quick-system-message (lambda (&rest _) "一句话解释：")))

(with-eval-after-load 'magit
  (add-hook 'magit-mode-hook #'gptel-magit-install))

(with-eval-after-load 'gptel-magit
  ;; (setq gptel-magit-backend (gptel-make-gemini "Gemini"
  ;; 					       :key 'gptel-api-key
  ;; 					       :stream t)
  ;; 	gptel-magit-models 'gemini-3.5-flash)
  (setq gptel-magit-body-length 72)
  (setq gptel-magit-commit-prompt (cdr (assoc "Conventional Commits" gptel-magit-commit-styles-alist))))


(provide 'init-ai)
