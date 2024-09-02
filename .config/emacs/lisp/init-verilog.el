;; init-verilog.el -*- lexical-binding: t -*-

(use-package verilog-mode
  :ensure nil
  :init
  (setq verilog-indent-level 4)
  (setq verilog-indent-level-module 0)
  (setq verilog-indent-level-declaration 0)
  (setq verilog-indent-level-behavioral 0)
  (setq verilog-indent-level-directive 0)
  (setq verilog-indent-lists nil)
  (setq verilog-cexp-indent 4)
  (setq verilog-case-indent 4)
  (setq verilog-auto-newline nil))
(use-package verilog-ext
  :hook ((verilog-mode . verilog-ext-mode))
  :init
  ;; Can also be set through `M-x RET customize-group RET verilog-ext':
  ;; Comment out/remove the ones you do not need
  (when ON-WINDOWS ;; on-linux verilator
    (setq verilog-ext-flycheck-linter 'verilog-iverilog)
    (add-to-list 'exec-path "c:/iverilog/bin")
    (setq flycheck-verilog-iverilog-executable "iverilog.exe"))
  (setq verilog-ext-feature-list
        '(font-lock
          xref
          capf
          hierarchy
          eglot
          ;; lsp
          flycheck
          beautify
          navigation
          template
          ;; formatter
          compilation
          imenu
          which-func
          hideshow
          typedefs
          time-stamp
          ;; block-end-comments
          ports))
  :config
  (verilog-ext-mode-setup))

(provide 'init-verilog)
;;; init-verilog.el ends here
