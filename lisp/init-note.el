;; -*- lexical-binding: t; -*-

(use-package denote
  :defer t
  :custom
  (denote-prompts '(title keywords signature))
  (denote-rename-confirmations nil)
  (denote-org-store-link-to-heading nil)
  (denote-directory (expand-file-name "denote" my-galaxy))
  (denote-rename-buffer-format "%b %t")
  (denote-rename-buffer-backlinks-indicator ""))

(use-package denote-org
  :after denote org
  :commands
  (denote-org-link-to-heading
   denote-org-backlinks-for-heading

   denote-org-extract-org-subtree

   denote-org-convert-links-to-file-type
   denote-org-convert-links-to-denote-type

   denote-org-dblock-insert-files
   denote-org-dblock-insert-links
   denote-org-dblock-insert-backlinks
   denote-org-dblock-insert-missing-links
   denote-org-dblock-insert-files-as-headings))

(use-package denote-journal
  :bind ("C-c n j" . denote-journal-new-or-existing-entry)
  :hook (calendar-mode . denote-journal-calendar-mode)
  :custom
  (denote-journal-directory
   (expand-file-name "journal" denote-directory))
  (denote-journal-keyword "journal"))

(use-package denote-explore
  :bind
  (;; Statistics
   ("C-c e s n" . denote-explore-count-notes)
   ("C-c e s k" . denote-explore-count-keywords)
   ("C-c e s e" . denote-explore-barchart-filetypes)
   ("C-c e s w" . denote-explore-barchart-keywords)
   ("C-c e s t" . denote-explore-barchart-timeline)
   ;; Random walks
   ("C-c e w n" . denote-explore-random-note)
   ("C-c e w r" . denote-explore-random-regex)
   ("C-c e w l" . denote-explore-random-link)
   ("C-c e w k" . denote-explore-random-keyword)
   ;; Denote Janitor
   ("C-c e j d" . denote-explore-duplicate-notes)
   ("C-c e j D" . denote-explore-duplicate-notes-dired)
   ("C-c e j l" . denote-explore-missing-links)
   ("C-c e j z" . denote-explore-zero-keywords)
   ("C-c e j s" . denote-explore-single-keywords)
   ("C-c e j r" . denote-explore-rename-keywords)
   ("C-c e j y" . denote-explore-sync-metadata)
   ("C-c e j i" . denote-explore-isolated-files)
   ;; Visualise denote
   ("C-c e n" . denote-explore-network)
   ("C-c e r" . denote-explore-network-regenerate)
   ("C-c e d" . denote-explore-barchart-degree)
   ("C-c e b" . denote-explore-barchart-backlinks))
  :custom
  (denote-explore-network-filename (expand-file-name "mindmap/denote-network.html" my-galaxy))
  (denote-explore-json-edges-filename (expand-file-name "denote-edges.json" cache-directory))
  (denote-explore-json-vertices-filename (expand-file-name "denote-vertices.json" cache-directory)))

(use-package consult-notes
  :bind ("C-c n f" . consult-notes)
  :hook (on-first-buffer . consult-notes-denote-mode)
  :custom
  (consult-notes-denote-files-function (lambda () (denote-directory-files nil t t))))

(provide 'init-note)
