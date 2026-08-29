;;; -*- lexical-binding: t -*-
(defvar my/used-packages
  '(
    avy
    benchmark-init-el
    browse-at-remote
    bufferlo
    biblio.el
    cape
    citar
    cond-let
    compat
    consult
    consult-dir
    consult-notes
    dash.el
    denote
    denote-journal
    denote-merge
    denote-org
    diredfl
    elisp-demos
    elisp-refs
    emacs-reader
    emacs-smart-input-source
    emacs-undo-fu-session
    Emacs-wgrep
    embark
    expreg
    f.el
    file-info
    form-feed
    gcmh
    gptel
    gptel-quick
    gptel-magit
    goggles
    helpful
    hungry-delete
    hydra
    liberime
    liberime-regexp
    llama
    marginalia
    magit
    nerd-icons-completion
    nerd-icons-dired
    nerd-icons-ibuffer
    nerd-icons.el
    on.el
    orderless
    org-edna
    org-gtd.el
    parsebib
    pdf-tools
    posframe
    popper
    rainbow-mode
    rg.el
    rimel
    s.el
    scihub
    saveplace-pdf-view
    selected.el
    surround
    symbol-overlay
    tablist
    vundo
    with-editor
    zotra
    ))

(defun my/add-used-packages-to-load-path ()
  "只把 my/used-packages 里的目录加入 load-path。"
  (let ((pkg-root (expand-file-name "packages" user-emacs-directory)))
    (dolist (name my/used-packages)
      (let ((dir (expand-file-name (symbol-name name) pkg-root)))
        (when (file-directory-p dir)
          (add-to-list 'load-path dir t)
          ;; 少数包有子目录，递归加入有 .el 的子目录
          (let ((default-directory dir))
            (normal-top-level-add-subdirs-to-load-path)))))))

(defmacro add-hook! (hooks &rest rest)
  "A convenience macro for adding N functions to M hooks.

This macro accepts, in order:

  1. The hook(s) to add to.
  2. Optional properties :local, :append, and/or :depth [N].
  3. The function(s) to be added: this can be a quoted function, a quoted list
     thereof, a list of `defun' or `cl-defun' forms, or arbitrary forms (will
     implicitly be wrapped in a lambda).

\(fn HOOKS [:append :local [:depth N] :remove :call-immediately :unless-daemonp-call-immediately] FUNCTIONS-OR-FORMS...)"
  (declare (indent defun))
  (let* ((hook-forms (if (listp hooks) hooks (list hooks)))
         (func-forms ())
         (defn-forms ())
         append-p local-p remove-p call-immediately-p unless-daemonp-call-immediately-p depth)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append-p t))
        (:depth  (setq depth (pop rest)))
        (:local  (setq local-p t))
        (:remove (setq remove-p t))
        (:call-immediately (setq call-immediately-p t))
        (:unless-daemonp-call-immediately
         (setq unless-daemonp-call-immediately-p t))))
    (while rest
      (let* ((next (pop rest))
             (first (car-safe next)))
        (push (cond ((memq first '(function nil)) next)
                    ((eq first 'quote)
                     (let ((quoted (cadr next)))
                       (if (atom quoted)
                           next
                         (when (cdr quoted)
                           (setq rest (cons (list first (cdr quoted)) rest)))
                         (list first (car quoted)))))
                    ((memq first '(defun cl-defun))
                     (push next defn-forms)
                     (list 'function (cadr next)))
                    ((prog1 `(lambda (&rest _) ,@(cons next rest))
                       (setq rest nil))))
              func-forms)))
    `(progn
       ,@defn-forms
       (dolist (func (list ,@func-forms))
         (dolist (hook ',(reverse hook-forms))
           ,(if remove-p
                `(remove-hook hook func ,local-p)
              `(add-hook hook func ,(or depth append-p) ,local-p)))
         ,(cond (call-immediately-p `(funcall func))
                (unless-daemonp-call-immediately-p
                 `(unless (daemonp)
                    (funcall func))))))))


(provide 'init-util)
