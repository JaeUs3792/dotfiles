;; init-rust.el --- Initialize Rust configurations.	-*- lexical-binding: t -*-

;; Rust

(use-package rust-mode
  :straight t
  :ensure t
  :defer t
  :config
  (flycheck-rust-setup))
(use-package cargo
  :straight t
  :ensure t
  :defer t
  :after rust-mode)
(use-package flycheck-rust
  :straight t
  :ensure t
  :after (flycheck rust-mode)
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
(use-package toml-mode
  :straight t
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("/\\(Cargo.lock\\|\\.cargo/config\\)\\'" . toml-mode)))

; org-babel integration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ob-rust
  :straight t
  :ensure t)

(provide 'init-rust)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rust.el ends here
