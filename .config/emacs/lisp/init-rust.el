;; init-rust.el --- Initialize Rust configurations.	-*- lexical-binding: t -*-

;; Rust

(use-package rust-mode
  :ensure t
  :defer t
  :config
  (flycheck-rust-setup))
(use-package cargo
  :ensure t
  :defer t
  :after rust-mode)
(use-package flycheck-rust
  :ensure t
  :after (flycheck rust-mode)
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
(use-package toml-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("/\\(Cargo.lock\\|\\.cargo/config\\)\\'" . toml-mode)))

; org-babel integration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ob-rust
  :ensure t)

(provide 'init-rust)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rust.el ends here
