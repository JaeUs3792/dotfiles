;;; init-ibuffer.el -*- lexical-binding: t -*-
(require 'init-const)
(require 'init-funcs)

(use-package ibuffer
  :ensure nil ; built-in
  :init
  (setq ibuffer-filter-group-name-face
        '(:inherit (font-lock-string-face bold)))
  :config
  ;; Use evil-collection-define-key, not :general, so these win over evil's
  ;; own default normal-state bindings for C-o/F/s (jump-backward,
  ;; find-char-backward, substitute), which take priority over the raw
  ;; major-mode keymap under evil.
  (evil-collection-define-key 'normal 'ibuffer-mode-map
    (kbd "C-o") 'casual-ibuffer-tmenu
    "F"         'casual-ibuffer-filter-tmenu
    "s"         'casual-ibuffer-sortby-tmenu))

(use-package nerd-icons-ibuffer
  :ensure t
  :defer t
  :hook
  (ibuffer-mode . nerd-icons-ibuffer-mode)
  :init
  (setq nerd-icons-ibuffer-icon t))
;; Group ibuffer's list by project
(use-package ibuffer-project
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
