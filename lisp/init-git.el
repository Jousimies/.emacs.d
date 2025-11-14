;; init-git.el --- Version manager. -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(use-package magit
  :load-path ("packages/magit/lisp" "packages/with-editor/lisp" "packages/llama/")
  :defer t
  :config
  (setq magit-git-executable "/usr/bin/git")
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-module-sections-hook 'magit-insert-modules-overview)
  (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpulled-from-pushremote)
  (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpushed-to-upstream)
  (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpushed-to-pushremote))

(use-package magit-status
  :bind ("C-x g" . magit-status-quick))

(with-eval-after-load 'transient
  (setq transient-history-file (expand-file-name "transient/history.el" cache-directory)
		transient-levels-file (expand-file-name "transient/levels.el" cache-directory)
		transient-values-file (expand-file-name "transient/values.el" cache-directory)))

(use-package git-timemachine
  :load-path "packages/git-timemachine/"
  :bind ("M-g t" . git-timemachine))

(use-package browse-at-remote
  :load-path "packages/browse-at-remote/"
  :bind ("M-g b" . browse-at-remote))

;; (use-package blamer
;;   :load-path "~/.emacs.d/packages/blamer.el/"
;;   :hook (prog-mode . blamer-mode)
;;   :custom
;;   (blamer-idle-time 0.3)
;;   (blamer-min-offset 70)
;;   :custom-face
;;   (blamer-face ((t :foreground "#7a88cf"
;;                    :background unspecified
;;                    :height 140
;;                    :italic t))))

(defun blog-git-auto-push ()
  "Automatically stage, commit, and push changes to the blog repository."
  (interactive)
  (let* ((blog-dir "~/blog/") ; 替换为你的博客目录
         (commit-message (format-time-string "Blog update %Y-%m-%d %H:%M:%S")))
    (save-some-buffers t) ; 保存所有未保存的缓冲区
    (shell-command (concat "cd " blog-dir " && git add ."))
    (shell-command (concat "cd " blog-dir " && git commit -m \"" commit-message "\""))
    (shell-command (concat "cd " blog-dir " && git push origin main"))
    (message "Blog changes staged, committed, and pushed!")))

;; 绑定到快捷键，例如 C-c g
(global-set-key (kbd "C-c b p") 'blog-git-auto-push)

(provide 'init-git)
;;; init-git.el ends here.
