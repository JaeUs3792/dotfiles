;;; init-dired.el -*- lexical-binding: t -*-
(require 'init-const)
(require 'init-funcs)

;; Directory operations
(use-package dired
  :ensure nil ; built-in
  :hook (dired-mode . dired-omit-mode)
  :general
  (:keymaps 'dired-mode-map
            "C-c C-p" 'wdired-change-to-wdired-mode)
  :custom (dired-omit-files (rx (seq bol ".")))
  :config
  ;; Guess a default target directory
  (setq dired-dwim-target t)

  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)

  ;; Show directory first
  (setq dired-listing-switches "-alh --group-directories-first")
  (if (executable-find "fd")
      (evil-collection-define-key 'normal 'dired-mode-map
        "f" 'fd-dired)
    (evil-collection-define-key 'normal 'dired-mode-map
      "f" 'find-dired))
  (evil-collection-define-key 'normal 'dired-mode-map
    "." 'dired-omit-mode
    "h" 'dired-up-directory
    "l" 'dired-find-file
    ")" 'dired-git-info-mode
    "s" 'my/dired-sort-menu
    "C" 'dired-rsync
    "z" 'my/dired-open-yazi))

(defun my/dired-open-yazi ()
  "Open yazi in the current dired directory using ghostty."
  (interactive)
  (let ((dir (dired-current-directory)))
    (start-process "yazi" nil "ghostty" "-e" "yazi" dir)))

(defun my/dired-sort-by (flags)
  "Sort dired buffer by FLAGS."
  (setq dired-listing-switches (concat "-alh --group-directories-first " flags))
  (dired-sort-other dired-listing-switches))

(require 'transient)
(transient-define-prefix my/dired-sort-menu ()
  "Dired sort menu."
  [["Sort by"
    ("n" "name"          (lambda () (interactive) (my/dired-sort-by ""))          :transient t)
    ("e" "extension"     (lambda () (interactive) (my/dired-sort-by "-X"))        :transient t)
    ("s" "size"          (lambda () (interactive) (my/dired-sort-by "-S"))        :transient t)
    ("t" "time modified" (lambda () (interactive) (my/dired-sort-by "-t"))        :transient t)
    ("c" "time created"  (lambda () (interactive) (my/dired-sort-by "-tc"))       :transient t)
    ("a" "time accessed" (lambda () (interactive) (my/dired-sort-by "-tu"))       :transient t)]
   ["Order"
    ("r" "reverse"       (lambda () (interactive) (my/dired-sort-by "-r"))        :transient t)]
   ["Quit"
    ("q" "quit" transient-quit-all)]])

;; Show git info in dired
(use-package dired-git-info
  :ensure t
  :defer t)

;; Allow rsync from dired buffers
(use-package dired-rsync
  :ensure t
  :defer t)

;; Colorful dired
(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

;; Shows icons
(use-package nerd-icons-dired
  :ensure t
  :diminish
  :commands nerd-icons-dired-mode
  :custom-face
  (nerd-icons-dired-dir-face ((t (:inherit nerd-icons-dsilver :foreground unspecified))))
  :hook (dired-mode . nerd-icons-dired-mode))

;; Extra Dired functionality
(use-package dired-aux ; built-in package
  :ensure nil)

;; `find-dired' alternative using `fd'
(when (executable-find "fd")
  (use-package fd-dired
    :ensure t
    :defer t))

(provide 'init-dired)
;;; init-dired.el ends here
