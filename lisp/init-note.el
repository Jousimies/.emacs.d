;; -*- lexical-binding: t; -*-

(use-package denote
  :ensure t
  :preface (package-activate 'denote)
  :defer t
  :custom
  (denote-prompts '(title keywords signature))
  (denote-rename-confirmations nil)
  (denote-org-store-link-to-heading nil)
  (denote-directory (expand-file-name "denote" my-galaxy))
  (denote-rename-buffer-format "%b %t")
  (denote-rename-buffer-backlinks-indicator ""))

(use-package denote-org
  :ensure t
  :preface (package-activate 'denote-org)
  :after denote)

(use-package denote-journal
  :ensure t
  :preface (package-activate 'denote-journal)
  :bind ("C-c n j" . denote-journal-new-or-existing-entry)
  :hook (calendar-mode . denote-journal-calendar-mode)
  :custom
  (denote-journal-directory
   (expand-file-name "journal" denote-directory))
  (denote-journal-keyword "journal"))

(use-package denote-explore
  :ensure t
  :preface (package-activate 'denote-explore)
  :after denote
  :custom
  (denote-explore-network-filename (expand-file-name "mindmap/denote-network.html" my-galaxy))
  (denote-explore-json-edges-filename (expand-file-name "denote-edges.json" cache-directory))
  (denote-explore-json-vertices-filename (expand-file-name "denote-vertices.json" cache-directory)))

(use-package consult-notes
  :ensure t
  :preface (package-activate 'consult-notes)
  :bind ("C-c n f" . consult-notes)
  :hook (minibuffer-setup . consult-notes-denote-mode)
  :custom
  (consult-notes-denote-files-function (lambda () (denote-directory-files nil t t))))

(provide 'init-note)
