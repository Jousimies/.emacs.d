;; -*- lexical-binding: t; -*-


(defconst cache-directory (expand-file-name ".cache" user-emacs-directory))



(when (string-equal (system-name) "DESKTOP-4SS4TL9")
  (defconst my-galaxy "D:/Nextcloud/L.Personal.Galaxy"))

(when (string-equal (system-name) "DESKTOP-TCLL527")
  (defconst my-galaxy "C:/Users/JGS/OneDrive/Galaxy/"))

(when (eq system-type 'darwin)
  (defconst my-galaxy "~/Nextcloud/L.Personal.Galaxy"))

(defconst my/project-folder "D:/1-Project/")

(provide 'init-vars)
