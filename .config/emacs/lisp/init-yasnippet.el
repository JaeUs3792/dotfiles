;;; init-yasnippet.el -*- lexical-binding: t -*-
;; Yet another snippet extension
(use-package yasnippet
  :ensure (:wait t)
  :demand t
  :diminish yas-minor-mode
  :config
  (setq yas-indent-line nil)
  (yas-global-mode)) ;; indent does not work properly.

;; Collection of yasnippet snippets
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
