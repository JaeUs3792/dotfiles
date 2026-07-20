;; init-debug -*- lexical-binding: t -*-
;; dape: eglot-native DAP client, no lsp-mode dependency
(use-package dape
  :ensure t
  :defer t
  :config
  (setq dape-buffer-window-arrangement 'right)
  (add-hook 'dape-start-hook
            (defun dape--save-on-start ()
              (save-some-buffers t t)))
  ;; Rust: run gdb via rust-gdb wrapper so Rust types pretty-print
  (add-to-list 'dape-configs
               `(rust-gdb
                 modes (rust-mode rust-ts-mode)
                 command "rust-gdb"
                 command-args ("--interpreter=dap")
                 :request "launch"
                 :program dape-buffer-default
                 :stopAtBeginningOfMainSubprogram nil)))


(provide 'init-debug)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-debug.el ends here
