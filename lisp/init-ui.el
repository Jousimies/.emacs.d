;; -*- lexical-binding: t; -*-

(setup (:package nerd-icons)
  (:when-loaded
    (when (display-graphic-p)
      (unless (find-font (font-spec :name nerd-icons-font-family))
        (nerd-icons-install-fonts t))
      (nerd-icons-set-font))))

(setup (:package nerd-icons-ibuffer)
  (:hook-into ibuffer-mode))

(setup (:package nerd-icons-dired)
  (:hook-into dired-mode))

(setup (:package nerd-icons-completion)
  (:hook-into minibuffer-mode))


(provide 'init-ui)
