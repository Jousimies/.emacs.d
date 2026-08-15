;; -*- lexical-binding: t; -*-
(global-set-key [remap list-buffers] #'ibuffer)
(add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)
(with-eval-after-load 'ibuffer
  (setq ibuffer-expert t
        ibuffer-show-empty-filter-groups nil
        ibuffer-default-sorting-mode 'major-mode))

(use-package bufferlo
  :ensure t
  :preface (package-activate 'bufferlo)
  :bind (([remap switch-to-buffer] . bufferlo-switch-to-buffer))
  :hook (on-first-buffer . bufferlo-mode))

(use-package helpful
  :ensure t
  :preface (package-activate 'helpful)
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key))
  :config
  (add-to-list 'display-buffer-alist '("\\*helpful"
                                       (display-buffer-in-side-window)
                                       (side . right)
                                       (window-width . 0.5)
                                       (window-parameters
                                        (mode-line-format . none)))))

(use-package elisp-demos
  :ensure t
  :preface (package-activate 'elisp-demos)
  :after helpful
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(provide 'init-buffer)
