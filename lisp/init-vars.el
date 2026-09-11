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

(defgroup my-paths nil
  "Locations used by this Emacs configuration."
  :group 'environment)

(defcustom my/project-folder
  (if sys/win32p
      "D:/1-Project/"
    (expand-file-name "Repositories/" "~"))
  "Root directory used by the project helper commands."
  :type 'directory
  :group 'my-paths)

(defconst onedrive
  (when sys/win32p
    (expand-file-name "OneDrive" (file-truename "~")))
  "Current user's OneDrive directory on Windows, or nil elsewhere.")

(defconst icloud/mobile
  (when sys/macp
    (expand-file-name "~/Library/Mobile Documents"))
  "Current user's iCloud Mobile Documents directory on macOS.")

(defcustom my-galaxy
  (if sys/win32p
      (expand-file-name "Galaxy" onedrive)
    (expand-file-name "~/Nextcloud/L.Personal.Galaxy"))
  "Root directory for personal notes, references, and GTD data."
  :type 'directory
  :group 'my-paths)

(defcustom my/org-gtd-directory
  (file-name-as-directory
   (if sys/macp
       (expand-file-name
        "iCloud~com~appsonthemove~beorg/Documents/org"
        icloud/mobile)
     (expand-file-name "gtd" my-galaxy)))
  "Directory used by Org GTD and Org Agenda."
  :type 'directory
  :group 'my-paths)

(defcustom my/inbox-file
  (expand-file-name "inbox.org" my/org-gtd-directory)
  "Primary Org inbox file."
  :type 'file
  :group 'my-paths)

(defcustom my/blog-source-directory
  (file-name-as-directory (expand-file-name "~/Repositories/blog-source"))
  "Local blog source repository used by GitHub Actions."
  :type 'directory
  :group 'my-paths)

;; Values used by progressively-loaded note/bibliography packages must be set
;; before their files are required.  Otherwise autoloaded commands such as
;; `citar-open' may see Denote/Citar defaults.
(defcustom my/denote-directory
  (file-name-as-directory (expand-file-name "denote" my-galaxy))
  "Directory containing Denote notes."
  :type 'directory
  :group 'my-paths)

(defcustom my/reference-lists
  (list (expand-file-name "bibtexs/My Library.bib" my-galaxy)
        (expand-file-name "bibtexs/Books.bib" my-galaxy)
	(expand-file-name "bibtexs/Seismic.bib" my-galaxy))
  "BibTeX files used by Org Cite and Citar."
  :type '(repeat file)
  :group 'my-paths)

(setq denote-directory my/denote-directory
      denote-journal-directory (expand-file-name "journal" my/denote-directory)
      org-cite-global-bibliography my/reference-lists
      citar-bibliography my/reference-lists
      citar-library-paths `(,(expand-file-name "PDF/" my-galaxy))
      citar-notes-paths `(,(expand-file-name "References" my/denote-directory)))


(provide 'init-vars)
