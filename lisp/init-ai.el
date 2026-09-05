;; -*- lexical-binding: t; -*-

(use-package gptel
  :commands gptel gptel-send
  :hook ((gptel-post-strem . gptel-auto-scroll)
	 (gptel-post-response-functions . gptel-end-of-response))
  :config
  (require 'gptel-openai-extras)
  (setq gptel-model 'deepseek-v4-flash
	gptel-backend (gptel-make-deepseek "DeepSeek"
			:stream t
			:key 'gptel-api-key)))

(use-package gptel-quick
  :after (gptel embark)
  :config
  (setq gptel-quick-backend (gptel-make-deepseek "DeepSeek"
			      :stream t
			      :key 'gptel-api-key)
        gptel-quick-model 'deepseek-v4-flash
        gptel-quick-word-count 500
        gptel-quick-system-message (lambda (&rest _) "一句话解释："))
  (keymap-set embark-general-map "?" #'gptel-quick))

(use-package gptel-magit
  :hook ((magit-mode . gptel-magit-install))
  :config
  (require 'gptel)
  (unless (featurep 'gptel-gemini)
    (require 'gptel-gemini))
  (setq gptel-magit-backend (gptel-make-gemini "Gemini"
					       :key 'gptel-api-key
					       :stream t)
	gptel-magit-models 'gemini-3.5-flash)
  (setq gptel-magit-body-length 72
	gptel-magit-commit-prompt (cdr (assoc "Conventional Commits" gptel-magit-commit-styles-alist))))



(provide 'init-ai)
