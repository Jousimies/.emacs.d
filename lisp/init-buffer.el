;;; init-buffer.el --- Buffer                        -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Duan Ning

;; Author: Duan Ning <duan_n@outlook.com>
;; Keywords:

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

(global-set-key (kbd "C-x C-b") #'ibuffer)

(add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)

(with-eval-after-load 'ibuffer
  (setopt ibuffer-default-sorting-mode 'major-mode))

(use-package bufferlo
  :load-path "packages/bufferlo/"
  :bind (([remap switch-to-buffer] . bufferlo-switch-to-buffer))
  :hook (after-init . bufferlo-mode))

(use-package helpful
  :load-path "packages/helpful/" "packages/elisp-refs/"
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key))
  :config
  (add-to-list 'display-buffer-alist '("\\*helpful"
                                         (display-buffer-in-side-window)
                                         (side . right)
                                         (window-width . 0.5)
                                         (window-parameters
                                          (mode-line-format . none)))))

(add-to-list 'load-path "~/.emacs.d/packages/elisp-refs/")
(add-to-list 'load-path "~/.emacs.d/packages/elisp-demos/")
(autoload #'elisp-demos-advice-helpful-update "elisp-demos" nil t)
(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

(use-package perspective
  :load-path "packages/perspective-el/"
  :bind (("M-s-n" . persp-switch)
         ("M-s-w" . persp-kill))
  :custom
  (persp-mode-prefix-key (kbd "C-c z"))
  :hook ((emacs-startup . persp-mode)
         (kill-emacs . persp-state-save))
  :config
  (setq persp-state-default-file (expand-file-name "persp" cache-directory))
  (with-eval-after-load 'tab-bar
    (defun tab-bar-format-persp ()
      (setq global-mode-string (delete '(:eval (persp-mode-line)) global-mode-string))
      `((global menu-item ,(format-mode-line (persp-mode-line)) ignore)))
    (add-to-list 'tab-bar-format 'tab-bar-format-persp)))

(use-package popper
  :load-path "packages/popper/"
  :bind (("C-`" . popper-toggle)
         :map popper-mode-map
         ("M-<tab>" . popper-cycle)
         ("M-`" . popper-toggle-type))
  :hook (emacs-startup . popper-mode)
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$" "\\*Pp Eval Output\\*$"
          "\\*Compile-Log\\*"
          "\\*Completions\\*"
          "\\*Warnings\\*"
          "\\*Async Shell Command\\*"
          "\\*Apropos\\*"
          "\\*Backtrace\\*"
          "\\*Embark Actions\\*"
          "\\*Finder\\*"
          "\\*Kill Ring\\*"
          "\\*Go-Translate\\*"

          bookmark-bmenu-mode
          comint-mode
          compilation-mode
          help-mode helpful-mode
          tabulated-list-mode
          Buffer-menu-mode

          gnus-article-mode devdocs-mode
          grep-mode occur-mode rg-mode ag-mode pt-mode
          youdao-dictionary-mode osx-dictionary-mode fanyi-mode

          "^\\*Process List\\*" process-menu-mode
          list-environment-mode cargo-process-mode
          "^\\*EKG Capture"
          "^\\*Ibuffer\\*" ibuffer-mode
          "^\\*eshell.*\\*.*$" eshell-mode
          "^\\*shell.*\\*.*$"  shell-mode
          "^\\*terminal.*\\*.*$" term-mode
          "^\\*vterm.*\\*.*$"  vterm-mode
          "^\\*eldoc.*\\*.*$" eldoc-mode

          "\\*Flycheck errors\\*$" " \\*Flycheck checker\\*$"
          "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
          "\\*[Wo]*Man.*\\*$"
          "\\*ert\\*$" overseer-buffer-mode
          "\\*gud-debug\\*$"
          "\\*lsp-help\\*$" "\\*lsp session\\*$"
          "\\*quickrun\\*$"
          "\\*tldr\\*$"
          "\\*vc-.*\\*$"
          "^\\*elfeed-entry\\*$"
          "^\\*macro expansion\\**"

          "\\*TeX Help\\*"
          "^\\*denote-backlinks to "
          "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Org Note\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"))
  :config
  (setq popper-mode-line '(:eval (propertize "POP" 'face `(:inverse-video t))))
  ;; Enable indicator in minibuffer
  (defun my/popper--fit-window-height (win)
    "Determine the height of popup window WIN by fitting it to the buffer's content."
    (fit-window-to-buffer
     win
     (floor (frame-height) 2)
     (floor (frame-height) 3)))
  (setq popper-window-height #'my/popper--fit-window-height)
  ;; HACK: close popper with `C-g'
  (defun +popper-close-window-hack (&rest _)
    "Close popper window via `C-g'."
    (when (and (called-interactively-p 'interactive)
               (not (region-active-p))
               popper-open-popup-alist)
      (let ((window (caar popper-open-popup-alist)))
        (when (window-live-p window)
          (delete-window window)))))
  (advice-add #'keyboard-quit :before #'+popper-close-window-hack))

(use-package popper-echo
  :hook (popper-mode . popper-echo-mode))

;; https://github.com/roife/.emacs.d/blob/323536f51674ef68cad78f72eef31c8b49795518/core/init-ibuffer.el#L8
(defun +ibuffer-visit-buffer-in-popper ()
    (interactive)
    (if (window-parameter nil 'window-side)
        (let ((win (selected-window)))
          (ibuffer-visit-buffer-other-window)
          (delete-window win))
      (ibuffer-visit-buffer)))

(with-eval-after-load 'ibuffer
  (define-key ibuffer-mode-map (kbd "RET") #'+ibuffer-visit-buffer-in-popper))

(provide 'init-buffer)
;;; init-buffer.el ends here
