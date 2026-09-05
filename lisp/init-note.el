;; -*- lexical-binding: t; -*-

;; denote
(with-eval-after-load 'denote
  (setq denote-prompts '(title keywords signature))
  (setq denote-rename-confirmations nil)
  (setq denote-org-store-link-to-heading nil)
  (setq denote-directory my/denote-directory)
  (setq denote-rename-buffer-format "%b %t")
  (setq denote-rename-buffer-backlinks-indicator ""))

;; denote-journal
(global-set-key (kbd "C-c n j") #'denote-journal-new-or-existing-entry)
(add-hook 'calendar-mode-hook #'denote-journal-calendar-mode)
(with-eval-after-load 'denote-journal
  (setq denote-journal-directory
        (expand-file-name "journal" my/denote-directory))
  (setq denote-journal-keyword "journal"))


;; (use-package denote-explore
;;   :bind
;;   (;; Statistics
;;    ("C-c e s n" . denote-explore-count-notes)
;;    ("C-c e s k" . denote-explore-count-keywords)
;;    ("C-c e s e" . denote-explore-barchart-filetypes)
;;    ("C-c e s w" . denote-explore-barchart-keywords)
;;    ("C-c e s t" . denote-explore-barchart-timeline)
;;    ;; Random walks
;;    ("C-c e w n" . denote-explore-random-note)
;;    ("C-c e w r" . denote-explore-random-regex)
;;    ("C-c e w l" . denote-explore-random-link)
;;    ("C-c e w k" . denote-explore-random-keyword)
;;    ;; Denote Janitor
;;    ("C-c e j d" . denote-explore-duplicate-notes)
;;    ("C-c e j D" . denote-explore-duplicate-notes-dired)
;;    ("C-c e j l" . denote-explore-missing-links)
;;    ("C-c e j z" . denote-explore-zero-keywords)
;;    ("C-c e j s" . denote-explore-single-keywords)
;;    ("C-c e j r" . denote-explore-rename-keywords)
;;    ("C-c e j y" . denote-explore-sync-metadata)
;;    ("C-c e j i" . denote-explore-isolated-files)
;;    ;; Visualise denote
;;    ("C-c e n" . denote-explore-network)
;;    ("C-c e r" . denote-explore-network-regenerate)
;;    ("C-c e d" . denote-explore-barchart-degree)
;;    ("C-c e b" . denote-explore-barchart-backlinks))
;;   :custom
;;   (denote-explore-network-filename (expand-file-name "mindmap/denote-network.html" my-galaxy))
;;   (denote-explore-json-edges-filename (expand-file-name "denote-edges.json" cache-directory))
;;   (denote-explore-json-vertices-filename (expand-file-name "denote-vertices.json" cache-directory)))

;; consult-notes
(with-eval-after-load 'consult-notes
  (setq consult-notes-denote-files-function (lambda () (denote-directory-files nil t t))))
(with-eval-after-load 'consult
  (consult-notes-denote-mode))

;; olivetti
(global-set-key (kbd "<f7>") #'olivetti-mode)
(with-eval-after-load 'olivetti
  (setq olivetti-body-width 0.62))


(provide 'init-note)
