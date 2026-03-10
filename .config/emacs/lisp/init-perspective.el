;;; init-perspective.el -*- lexical-binding: t -*-

(use-package perspective
  :ensure (:wait t)
  :demand t
  :bind
  ("C-x C-b" . persp-list-buffers)
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  (persp-state-default-file (expand-file-name "perspectives" user-emacs-directory))
  (persp-sort 'access)
  (persp-modestring-short t)
  :hook
  (kill-emacs . persp-state-save)
  :init
  (persp-mode))

;; project.el integration
(use-package perspective-project-bridge
  :ensure (:wait t)
  :after perspective
  :config
  (perspective-project-bridge-mode))

;; Consult integration — show only current perspective buffers
(with-eval-after-load 'consult
  (consult-customize consult-source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source))

(provide 'init-perspective)
;;; init-perspective.el ends here
