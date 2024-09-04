;; init-rust.el --- Initialize Rust configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2022 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Rust configurations.
;;

;;; Code:

;; Rust
(use-package rustic
  :straight t
  :ensure t
  :defer t
  :init
  (if ON-LINUX
      (setq rustic-analyzer-command '("~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/bin/rust-analyzer"))
    (setq rustic-analyzer-command '("~/.rustup/toolchains/stable-x86_64-pc-windows-msvc/bin/rust-analyzer.exe")))
  (setq rustic-lsp-client 'eglot))
(use-package rust-playground
  :straight t
  :ensure t
  :defer t)
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
