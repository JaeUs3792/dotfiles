;;; init-verilog.el -*- lexical-binding: t -*-

(use-package verilog-mode
  :ensure nil ; built-in
  :init
  (setq verilog-indent-level             4
        verilog-indent-level-module      0
        verilog-indent-level-declaration 0
        verilog-indent-level-behavioral  0
        verilog-indent-level-directive   0
        verilog-indent-lists             nil
        verilog-cexp-indent              4
        verilog-case-indent              4
        verilog-auto-newline             nil))

(use-package verilog-ext
  :ensure (:wait t)
  :demand t
  :hook ((verilog-mode . verilog-ext-mode)
         (verilog-mode . eglot-ensure))
  :init
  (put 'verilog-ext-project-name 'safe-local-variable #'stringp)
  (put 'verilog-ext-project-dirs 'safe-local-variable #'listp)
  (put 'verilog-ext-project-ignore-dirs 'safe-local-variable #'listp)
  (setq verilog-ext-feature-list
        '(font-lock
          xref
          capf
          hierarchy
          eglot
          flycheck
          beautify
          navigation
          template
          compilation
          imenu
          which-func
          hideshow
          typedefs
          time-stamp
          tags
          ports))
  :config
  (setq verilog-ext-eglot-default-server 've-verible-ls)

  ;; Auto-register project from .dir-locals.el
  (defvar-local verilog-ext-project-name nil)
  (defvar-local verilog-ext-project-dirs nil)
  (defvar-local verilog-ext-project-ignore-dirs nil)

  (defun verilog-ext-auto-register-project ()
    "Auto-register current project in `verilog-ext-project-alist' from .dir-locals.el."
    (when (derived-mode-p 'verilog-mode)
      (when-let* ((root (or (when-let ((proj (project-current)))
                              (project-root proj))
                            (vc-root-dir)))
                  (root (expand-file-name root))
                  (name (or verilog-ext-project-name
                            (file-name-nondirectory (directory-file-name root)))))
        (let ((existing (assoc name verilog-ext-project-alist)))
          (when existing
            (setq verilog-ext-project-alist (delete existing verilog-ext-project-alist)))
          (let ((entry (list name :root root)))
            (when verilog-ext-project-dirs
              (setq entry (append entry (list :dirs verilog-ext-project-dirs))))
            (when verilog-ext-project-ignore-dirs
              (setq entry (append entry (list :ignore-dirs verilog-ext-project-ignore-dirs))))
            (push entry verilog-ext-project-alist)
            (message "[verilog-ext] auto-registered project: %s (%s)" name root))))))
  (add-hook 'hack-local-variables-hook #'verilog-ext-auto-register-project)

  (verilog-ext-mode-setup))

;; xref: verilog-ext backend takes priority over eglot
(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook
            (defun verilog-ext-xref-priority-h ()
              (when (derived-mode-p 'verilog-mode)
                (setq-local xref-backend-functions
                            '(verilog-ext-xref-backend eglot-xref-backend t))))))

(provide 'init-verilog)
;;; init-verilog.el ends here
