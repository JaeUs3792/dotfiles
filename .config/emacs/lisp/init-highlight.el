;;; init-highlight.el -*- lexical-binding: t -*-
(require 'init-const)
(require 'init-funcs)

(use-package hl-line
  :ensure nil
  :hook ((after-init . global-hl-line-mode)
         ((dashboard-mode eshell-mode shell-mode term-mode vterm-mode) .
          (lambda () (setq-local global-hl-line-mode nil)))))

(provide 'init-highlight)
;;; init-highlight.el ends here
