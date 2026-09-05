;; -*- lexical-binding: t; -*-


(defconst cache-directory (expand-file-name ".cache" user-emacs-directory))

(defconst sys/win32p(eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/macp (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst emacs/>=31p (>= emacs-major-version 31)
  "Emacs is 31 or above.")

;; (when (string-equal (system-name) "DESKTOP-4SS4TL9")
;;   (defconst my-galaxy "D:/Nextcloud/L.Personal.Galaxy"))

;; (when (string-equal (system-name) "DESKTOP-TCLL527")
;;   (defconst my-galaxy "C:/Users/JGS/OneDrive/Galaxy/"))

;; (when (eq system-type 'darwin)
;;   (defconst my-galaxy "~/Nextcloud/L.Personal.Galaxy"))

(defconst my/project-folder "D:/1-Project/")

(when (eq system-type 'darwin)
  (defconst my-galaxy "~/Nextcloud/L.Personal.Galaxy")
  (defconst icloud/mobile "~/Library/Mobile Documents"
    "Mobile Documents in Icloud.")
  (defconst my/org-gtd-directory (expand-file-name "iCloud~com~appsonthemove~beorg/Documents/org" icloud/mobile)))

(when (eq system-type 'windows-nt)
  (defconst onedrive (expand-file-name "OneDrive" (file-truename "~")))
  (defconst my-galaxy (expand-file-name "Galaxy" onedrive))
  (defconst my/org-gtd-directory (expand-file-name "gtd" my-galaxy))
  (defconst my/inbox-file (expand-file-name "inbox.org" my/org-gtd-directory)))


(provide 'init-vars)
