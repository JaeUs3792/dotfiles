;;; init-treemacs.el -*- lexical-binding: t -*-
(require 'init-const)
(require 'init-funcs)
;; A tree layout file explorer
(use-package treemacs
  :ensure (:wait t)
  :defer t
  :commands (treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-git-mode)
  :custom-face
  (cfrs-border-color ((t (:inherit posframe-border))))
  :bind (([f8]        . treemacs)
         :map treemacs-mode-map
         ([mouse-1]   . treemacs-single-click-expand-action))
  :config
  (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
        treemacs-missing-project-action  'remove
        treemacs-sorting                 'alphabetic-asc
        treemacs-follow-after-init       t
        treemacs-width                   30
        treemacs-no-png-images           nil)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple))))

(use-package treemacs-nerd-icons
  :ensure t
  :defer t
  :custom-face
  (treemacs-nerd-icons-root-face ((t (:inherit nerd-icons-green :height 1.3))))
  (treemacs-nerd-icons-file-face ((t (:inherit nerd-icons-dsilver))))
  :config (treemacs-load-theme "nerd-icons"))

(with-eval-after-load 'magit
  (use-package treemacs-magit
    :ensure t
    :defer t
    :config
    (add-hook 'magit-post-commit-hook #'treemacs-magit--schedule-update)
    (add-hook 'git-commit-post-finish-hook #'treemacs-magit--schedule-update)
    (add-hook 'magit-post-stage-hook #'treemacs-magit--schedule-update)
    (add-hook 'magit-post-unstage-hook #'treemacs-magit--schedule-update)))

(with-eval-after-load 'perspective
  (use-package treemacs-perspective
    :ensure t
    :demand t
    :config (treemacs-set-scope-type 'Perspectives)))
(provide 'init-treemacs)
;;; init-treemacs.el ends here
