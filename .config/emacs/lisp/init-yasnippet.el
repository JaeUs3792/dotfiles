;;; init-yasnippet.el -*- lexical-binding: t -*-
;; Yet another snippet extension
(use-package yasnippet
  :straight t
  :ensure t
  :defer t
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :config
  (setq yas-indent-line nil)) ;; indent does not work properly.

;; Collection of yasnippet snippets
(use-package yasnippet-snippets
  :straight t
  :ensure t)

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
