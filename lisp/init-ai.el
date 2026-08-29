;; -*- lexical-binding: t; -*-

(use-package gptel
  :commands gptel gptel-send
  :hook ((gptel-post-strem . gptel-auto-scroll)
	 (gptel-post-response-functions . gptel-end-of-response))
  :config
  (require 'gptel-openai-extras)
  (defun my/gptel-key-function ()
    (auth-source-pick-first-password :host "Deepseek" :user "Deepseek"))
  (setq gptel-model 'deepseek-v4-flash
	gptel-backend (gptel-make-deepseek "DeepSeek"
			:stream t
			:key (my/gptel-key-function))))

(use-package gptel-quick
  :after (gptel embark)
  :config
  (setq gptel-quick-backend (gptel-make-deepseek "DeepSeek"
			      :stream t
			      :key (my/gptel-key-function))
        gptel-quick-model 'deepseek-v4-flash
        gptel-quick-word-count 500
        gptel-quick-system-message (lambda (&rest _) "一句话解释："))
  (keymap-set embark-general-map "?" #'gptel-quick))

(use-package gptel-magit
  :hook ((magit-mode . gptel-magit-install))
  :config
  (require 'gptel)
  (setq gptel-magit-body-length 72))
        ;; gptel-magit-commit-prompt (cdr (assoc "Conventional Commits" gptel-magit-commit-styles-alist))))



(provide 'init-ai)
