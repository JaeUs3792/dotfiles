;;; init-dired.el -*- lexical-binding: t -*-
(require 'init-const)
(require 'init-funcs)

;; Directory operations
(use-package dired
  :ensure nil
  :general
  (:keymaps 'dired-mode-map
            "C-c C-p" 'wdired-change-to-wdired-mode)
  :config
  ;; Guess a default target directory
  (setq dired-dwim-target t)

  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)

  ;; Show directory first
  (setq dired-listing-switches "-alh --group-directories-first")

  ;; Quick sort dired buffers via hydra
  (use-package dired-quick-sort
    :hook (after-init . dired-quick-sort-setup)
    :init
    (setq dired-quick-sort-suppress-setup-warning t)
    :config
    (evil-collection-define-key 'normal 'dired-mode-map
      "s" 'hydra-dired-quick-sort/body))

  ;; Show git info in dired
  (use-package dired-git-info)

  ;; Allow rsync from dired buffers
  (use-package dired-rsync
    :bind (:map dired-mode-map
           ("C-c C-r" . dired-rsync)))

  ;; Colorful dired
  (use-package diredfl
    :hook (dired-mode . diredfl-mode))

  ;; Shows icons
  (use-package nerd-icons-dired
    :diminish
    :commands nerd-icons-dired-mode
    :custom-face
    (nerd-icons-dired-dir-face ((t (:inherit nerd-icons-dsilver :foreground unspecified))))
    :hook (dired-mode . nerd-icons-dired-mode))

  ;; Extra Dired functionality
  (use-package dired-aux :ensure nil)

  (use-package dired-single
    :commands (dired dired-jump))
  (use-package dired-hide-dotfiles
    :hook (dired-mode) ;; hide default when dired-mode enabled.
    :config
    (evil-collection-define-key 'normal 'dired-mode-map
      "H" 'dired-hide-dotfiles-mode))
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer
    ")" 'dired-git-info-mode))

;; `find-dired' alternative using `fd'
(when (executable-find "fd")
  (use-package fd-dired))

(provide 'init-dired)
;;; init-dired.el ends here
