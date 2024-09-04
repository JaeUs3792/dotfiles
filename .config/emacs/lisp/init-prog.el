;; init-prog.el -*- lexical-binding: t -*-
(require 'init-custom)
(require 'init-const)
(require 'init-funcs)

;; Tree-sitter support
;; (use-package treesit-auto
;;   :hook (after-init . global-treesit-auto-mode)
;;   :init (setq treesit-auto-install 'prompt))

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
  :straight t
  :ensure t
  :defer t
  :pretty-hydra
  ((:title (pretty-hydra-title "Dump Jump" 'faicon "nf-fa-anchor")
           :color blue :quit-key ("q" "C-g"))
   ("Jump"
    (("j" dumb-jump-go "Go")
     ("o" dumb-jump-go-other-window "Go other window")
     ("e" dumb-jump-go-prefer-external "Go external")
     ("x" dumb-jump-go-prefer-external-other-window "Go external other window"))
    "Other"
    (("i" dumb-jump-go-prompt "Prompt")
     ("l" dumb-jump-quick-look "Quick look")
     ("b" dumb-jump-back "Back"))))
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
         ;;("C-M-j" . dumb-jump-hydra/body))
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-prefer-searcher 'rg
        dumb-jump-selector 'ivy))

;; Code styles
;; (use-package editorconfig
;;   :straight t
;;   :ensure t
;;   :defer t
;;   :diminish
;;   :hook (after-init . editorconfig-mode))

;; Run commands quickly
(use-package quickrun
  :straight t
  :ensure t
  :defer t
  :bind (("C-<f5>" . quickrun)
         ("C-c X"  . quickrun)))

;; Browse devdocs.io documents using EWW
(use-package devdocs
  :straight t
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
      (python-mode     . ("python~3.10" "python~2.7"))
      (ruby-mode       . ("ruby~3.1"))
      (go-mode         . ("go"))
      (rustic-mode     . ("rust"))
      (css-mode        . ("css"))
      (html-mode       . ("html"))
      (julia-mode      . ("julia~1.8"))
      (js-mode         . ("javascript" "jquery"))
      (js2-mode        . ("javascript" "jquery"))
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
(use-package csv-mode
  :straight t
  :ensure t
  :defer t)
(use-package csharp-mode
  :straight t
  :ensure t
  :defer t)
(use-package cmake-mode
  :straight t
  :ensure t
  :defer t)
(use-package lua-mode
  :straight t
  :ensure t
  :defer t)

;; Windows Batch Mode eXtras
(use-package bmx-mode
  :straight t
  :ensure t
  :defer t
  :after company
  :diminish
  :hook (after-init . bmx-mode-setup-defaults))

; arch PKGBUILD
(use-package pkgbuild-mode
  :straight t
  :ensure t
  :defer t)
(use-package systemd
  :straight t
  :ensure t
  :defer t)


(provide 'init-prog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
