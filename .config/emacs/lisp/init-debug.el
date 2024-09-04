;; init-debug -*- lexical-binding: t -*-
;; (use-package dap-mode
;;   :config
;;   (setq dap-cpptools-extension-version "1.5.1")

;;   (with-eval-after-load 'lsp-rust
;;     (require 'dap-cpptools))

;;   ;; rust configuration
;;   (with-eval-after-load 'dap-cpptools
;;     ;; Add a template specific for debugging Rust programs.
;;     ;; It is used for new projects, where I can M-x dap-edit-debug-template
;;     (dap-register-debug-template "Rust::CppTools Run Configuration"
;;                                  (list :type "cppdbg"
;;                                        :request "launch"
;;                                        :name "Rust::Run"
;;                                        :MIMode "gdb"
;;                                        :miDebuggerPath "rust-gdb"
;;                                        :environment []
;;                                        :program "${workspaceFolder}/target/debug/hello / replace with binary"
;;                                        :cwd "${workspaceFolder}"
;;                                        :console "external"
;;                                        :dap-compilation "cargo build"
;;                                        :dap-compilation-dir "${workspaceFolder}")))

;;   (with-eval-after-load 'dap-mode
;;     (setq dap-default-terminal-kind "integrated") ;; Make sure that terminal programs open a term for I/O in an Emacs buffer
;;     (dap-auto-configure-mode +1)))

(use-package dap-mode
  :straight t
  :ensure t
  :defer t
  :config
  ;; Enabling only some features
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (require 'dap-gdb-lldb)
  (require 'dap-python)
  ;; configure debugger
  (setq dap-python-debugger 'debugpy)
  )


(provide 'init-debug)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-debug.el ends here
