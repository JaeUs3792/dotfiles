;;; init-ibuffer.el -*- lexical-binding: t -*-
(require 'init-const)
(require 'init-funcs)

(use-package ibuffer
  :ensure nil ; built-in
  :init
  (setq ibuffer-filter-group-name-face
        '(:inherit (font-lock-string-face bold))))
(use-package nerd-icons-ibuffer
  :straight t
  :ensure t
  :defer t
  :hook
  (ibuffer-mode . nerd-icons-ibuffer-mode)
  :init
  (setq nerd-icons-ibuffer-icon t))
;; Group ibuffer's list by project
(use-package ibuffer-project
  :straight t
  :ensure t
  :defer t
  :hook
  (ibuffer . (lambda ()
               (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
               (unless (eq ibuffer-sorting-mode 'project-file-relative)
                 (ibuffer-do-sort-by-project-file-relative))))
  :init
  (setq ibuffer-project-use-cache t)
  :config
  (add-to-list 'ibuffer-project-root-functions '(file-remote-p . "Remote"))
  (add-to-list 'ibuffer-project-root-functions '("\\*.+\\*" . "Default")))

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
