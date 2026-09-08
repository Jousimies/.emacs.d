;; -*- lexical-binding: t; -*-

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

(defconst cache-directory (expand-file-name ".cache" user-emacs-directory))

(defcustom my/project-folder
  (if sys/win32p
      "D:/1-Project/"
    (expand-file-name "Repositories/" "~"))
  "Root directory used by the project helper commands."
  :type 'directory
  :group 'convenience)

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

(defconst website-directory "~/Repositories/blog-source/")

;; Values used by progressively-loaded note/bibliography packages must be set
;; before their files are required.  Otherwise autoloaded commands such as
;; `citar-open' may see Denote/Citar defaults.
(defconst my/denote-directory (expand-file-name "denote" my-galaxy))
(defconst my/reference-lists `(,(expand-file-name "bibtexs/My Library.bib" my-galaxy)
                               ,(expand-file-name "bibtexs/Books.bib" my-galaxy)
			       ,(expand-file-name "bibtexs/Seismic.bib" my-galaxy)))

(setq denote-directory my/denote-directory
      denote-journal-directory (expand-file-name "journal" my/denote-directory)
      org-cite-global-bibliography my/reference-lists
      citar-bibliography my/reference-lists
      citar-library-paths `(,(expand-file-name "PDF/" my-galaxy))
      citar-notes-paths `(,(expand-file-name "References" my/denote-directory)))


(provide 'init-vars)
