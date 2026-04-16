;; init-rust.el --- Initialize Rust configurations.	-*- lexical-binding: t -*-

;; Rust
;; rust-ts-mode: Emacs 30.2 + tree-sitter 0.26 조합 버그로 모드 진입 자체 실패
;; rust-mode를 major mode로 사용, eglot + rust-analyzer가 highlighting 담당

(use-package rust-mode
  :ensure t
  :defer t
  :hook (rust-mode . (lambda () (flycheck-mode -1))))

;; eglot (built-in LSP) + rust-analyzer
;; (use-package eglot
;;   :hook (rust-mode . eglot-ensure)
;;   :config
;;   (add-to-list 'eglot-server-programs
;;                '(rust-mode . ("rust-analyzer"))))

;; (use-package cargo
;;   :ensure t
;;   :hook (rust-mode . cargo-minor-mode))

(provide 'init-rust)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rust.el ends here
