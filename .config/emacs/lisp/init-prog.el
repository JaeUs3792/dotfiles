;; init-prog.el -*- lexical-binding: t -*-
(require 'init-custom)
(require 'init-const)
(require 'init-funcs)

;; Tree-sitter support
(use-package treesit-auto
  :ensure t
  :demand t
  :config
  (setq treesit-auto-install 'prompt)
  (setq treesit-auto-langs (delq 'verilog treesit-auto-langs))
  (global-treesit-auto-mode))

;; Search tool
(use-package grep
  :ensure nil ; built-in
  :autoload grep-apply-setting
  :config
  (cond
   ((executable-find "ugrep")
    (grep-apply-setting
     'grep-command "ugrep --color=auto -0In -e ")
    (grep-apply-setting
     'grep-template "ugrep --color=auto -0In -e <R> <D>")
    (grep-apply-setting
     'grep-find-command '("ugrep --color=auto -0Inr -e ''" . 30))
    (grep-apply-setting
     'grep-find-template "ugrep <C> -0Inr -e <R> <D>"))
   ((executable-find "rg")
    (grep-apply-setting
     'grep-command "rg --color=auto --null -nH --no-heading -e ")
    (grep-apply-setting
     'grep-template "rg --color=auto --null --no-heading -g '!*/' -e <R> <D>")
    (grep-apply-setting
     'grep-find-command '("rg --color=auto --null -nH --no-heading -e ''" . 38))
    (grep-apply-setting
     'grep-find-template "rg --color=auto --null -nH --no-heading -e <R> <D>"))))

;;;; Cross-referencing commands
(use-package xref
  :ensure nil ; built-in
  :config
  (with-no-warnings
    ;; Use faster search tool
    (add-to-list 'xref-search-program-alist
                 '(ugrep . "xargs -0 ugrep <C> --null -ns -e <R>"))
    (cond
     ((executable-find "ugrep")
      (setq xref-search-program 'ugrep))
     ((executable-find "rg")
      (setq xref-search-program 'ripgrep)))

    ;; Select from xref candidates with Ivy
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read
          xref-show-xrefs-function #'xref-show-definitions-completing-read)))

;;;; Jump to definition
(use-package dumb-jump
  :ensure t
  :defer t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-prefer-searcher 'rg
        dumb-jump-selector 'completing-read))

;; Code styles
;; (use-package editorconfig
;;   :straight t
;;   :ensure t
;;   :defer t
;;   :diminish
;;   :hook (after-init . editorconfig-mode))

;; Run commands quickly
(use-package quickrun
  :ensure t
  :defer t
  :bind (("C-<f5>" . quickrun)
         ("C-c X"  . quickrun)))

;; Browse devdocs.io documents using EWW
(use-package devdocs
  :ensure t
  :defer t
  :autoload (devdocs--installed-docs devdocs--available-docs)
  :bind (:map prog-mode-map
              ("M-<f1>" . devdocs-dwim)
              ("C-h D"  . devdocs-dwim))
  :init
  (defconst devdocs-major-mode-docs-alist
    '((c-mode          . ("c"))
      (c++-mode        . ("cpp"))
      (python-mode     . ("python~3.10"))
      (ruby-mode       . ("ruby~3.1"))
      (go-mode         . ("go"))
      (rustic-mode     . ("rust"))
      (julia-mode      . ("julia~1.8"))
      (emacs-lisp-mode . ("elisp")))
    "Alist of major-mode and docs.")

  (mapc
   (lambda (mode)
     (add-hook (intern (format "%s-hook" (car mode)))
               (lambda ()
                 (setq-local devdocs-current-docs (cdr mode)))))
   devdocs-major-mode-docs-alist)

  (setq devdocs-data-dir (expand-file-name "devdocs" user-emacs-directory))

  (defun devdocs-dwim()
    "Look up a DevDocs documentation entry.

Install the doc if it's not installed."
    (interactive)
    ;; Install the doc if it's not installed
    (mapc
     (lambda (slug)
       (unless (member slug (let ((default-directory devdocs-data-dir))
                              (seq-filter #'file-directory-p
                                          (when (file-directory-p devdocs-data-dir)
                                            (directory-files "." nil "^[^.]")))))
         (mapc
          (lambda (doc)
            (when (string= (alist-get 'slug doc) slug)
              (devdocs-install doc)))
          (devdocs--available-docs))))
     (alist-get major-mode devdocs-major-mode-docs-alist))

    ;; Lookup the symbol at point
    (devdocs-lookup nil (thing-at-point 'symbol t))))

;;;; Misc. programming modes
;; (use-package csv-mode
;;   :ensure t
;;   :defer t)
;; (use-package csharp-mode
;;   :ensure t
;;   :defer t)
;; (use-package cmake-mode
;;   :ensure t
;;   :defer t)
;; (use-package lua-mode
;;   :ensure t
;;   :defer t)

;; Windows Batch Mode eXtras
;; (use-package bmx-mode
;;   :ensure t
;;   :defer t
;;   :diminish
;;   :hook (after-init . bmx-mode-setup-defaults))

; arch PKGBUILD
;; (use-package pkgbuild-mode
;;   :ensure t
;;   :defer t)
;; (use-package systemd
;;   :ensure t
;;   :defer t)

(provide 'init-prog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
