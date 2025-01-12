;;; init.el --- Emacs Initial Configuration          -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Duan Ning

;; Author: Duan Ning <duan_n@outlook.com>
;; Keywords: files

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(add-to-list 'load-path "~/.emacs.d/lisp")

;; Straight
(setq straight-check-for-modifications nil                   ; skip modification
      straight-vc-git-default-clone-depth '(1 single-branch) ; shadow clone
      comp-deferred-compilation-deny-list ()                 ; config native comp
      warning-suppress-log-types '((comp))                   ; Don't display comp warnings
      straight-disable-native-compile (not (and (fboundp 'native-comp-available-p)
                                                (native-comp-available-p))))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; For debug.
(when init-file-debug
  (setq use-package-compute-statistics t)
  (setq use-package-minimum-reported-time 0)
  (setq use-package-verbose t))

(require 'init-const)
(require 'init-core)
(require 'init-builtin)

;; UI
(require 'init-dashboard)
(require 'init-tab)
(require 'init-modeline)
(require 'init-buffer)
;; (require 'init-svg-tag)

;; Plateform related configuration
(when (eq system-type 'darwin)
  (require 'init-mac))

;; Better Editor
(require 'init-edit)
(require 'init-completion)
(require 'init-search)

;; File manager
(require 'init-dired)

;; Programming
(require 'init-lsp)
(require 'init-prog)
(require 'init-shell)
(require 'init-git)

;; Note Everything based on org-mode
(require 'init-org)
(require 'init-note)
(require 'init-bib)
(require 'init-latex)
(require 'init-gtd)
(require 'init-finance)
(require 'init-blog)

;; Reading with Emacs
(require 'init-reader)
(require 'init-dict)

;; Applications
(require 'init-telega)
(require 'init-mail)
(require 'init-elfeed)
;; (require 'init-pass)

;; Some useful functions stealed from Internet
(require 'init-misc)

;; Keybindings
(require 'init-keys)

;; Load custom.el, but It's empty.
 (load (setq custom-file (locate-user-emacs-file "custom.el")) t)

(provide 'init)
;;; init.el ends here
