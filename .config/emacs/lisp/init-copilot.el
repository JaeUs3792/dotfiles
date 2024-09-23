;; init-copilot.el -*- lexical-binding: t -*-

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t)
  ;; :hook (prog-mode . copilot-mode))

(provide 'init-copilot)
;;; init-copilot.el ends here
