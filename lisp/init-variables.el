;;; init-variables.el --- Variables. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; Define variables.
(defvar my-cloud "~/Nextcloud"
  "This folder is My cloud.")
(defvar my-galaxy (expand-file-name "L.Personal.Galaxy" my-cloud)
  "This folder stores all the plain text files of my life.")
(defvar website-directory "~/Nextcloud/L.Personal.Galaxy/website"
  "The source folder of my blog")
(defvar my/publish-directory "~/shuyi.github.io")

;;
(when (maybe-require-package 'no-littering)
  (require 'no-littering))

(provide 'init-variables)
;;; init-variables.el ends here
