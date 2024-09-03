;;; init-dired.el -*- lexical-binding: t -*-
(require 'init-const)
(require 'init-funcs)

;; Directory operations
(use-package dired
  :ensure nil ; built-in
  :hook (dired-mode . dired-omit-mode)
  :general
  (:keymaps 'dired-mode-map
            "C-c C-p" 'wdired-change-to-wdired-mode
            "C-c C-r" 'dired-rsync)
  :custom (dired-omit-files (rx (seq bol ".")))
  :config
  ;; Guess a default target directory
  (setq dired-dwim-target t)

  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)

  ;; Show directory first
  (setq dired-listing-switches "-alh --group-directories-first")
  (evil-collection-define-key 'normal 'dired-mode-map
                              "." 'dired-omit-mode
                              "h" 'dired-single-up-directory
                              "l" 'dired-single-buffer
                              ")" 'dired-git-info-mode
                              "s" 'hydra-dired-quick-sort/body))

;; Quick sort dired buffers via hydra
(use-package dired-quick-sort
  :straight t
  :ensure t
  :defer t
  :hook (after-init . dired-quick-sort-setup)
  :init
  (setq dired-quick-sort-suppress-setup-warning t))

;; Show git info in dired
(use-package dired-git-info
  :straight t
  :ensure t
  :defer t)

;; Allow rsync from dired buffers
(use-package dired-rsync
  :straight t
  :ensure t
  :defer t)

;; Colorful dired
(use-package diredfl
  :straight t
  :ensure t
  :hook (dired-mode . diredfl-mode))

;; Shows icons
(use-package nerd-icons-dired
  :straight t
  :ensure t
  :diminish
  :commands nerd-icons-dired-mode
  :custom-face
  (nerd-icons-dired-dir-face ((t (:inherit nerd-icons-dsilver :foreground unspecified))))
  :hook (dired-mode . nerd-icons-dired-mode))

;; Extra Dired functionality
(use-package dired-aux ; built-in package
             :ensure nil)

(use-package dired-single
             :straight t
             :ensure t
             :defer t
  :commands (dired dired-jump))


;; `find-dired' alternative using `fd'
(when (executable-find "fd")
  (use-package fd-dired
    :straight t
    :ensure t
    :defer t))

(provide 'init-dired)
;;; init-dired.el ends here
