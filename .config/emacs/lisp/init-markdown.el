;;; init-markdown.el -*- lexical-binding: t -*-
;; using multimarkdown compiler, could be found at AUR
(use-package markdown-mode
  :straight t
  :ensure t
  :defer t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  (setq markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-uppercase-checkbox t
        markdown-fontify-code-blocks-natively t))

(use-package grip-mode
  :straight t
  :ensure t
  :defer t
  ;; :config (setq grip-use-mdopen t) ;; to use `mdopen` instead of `grip`
  :bind (:map markdown-mode-command-map
         ("g" . grip-mode)))

(provide 'init-markdown)

;;; init-markdown.el ends here
