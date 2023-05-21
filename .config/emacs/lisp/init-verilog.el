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


(provide 'init-verilog)
;;; init-verilog.el ends here
