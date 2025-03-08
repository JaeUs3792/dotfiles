;; init-copilot.el -*- lexical-binding: t -*-

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t)
  ;; :hook (prog-mode . copilot-mode))

(use-package copilot-chat
  :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :after (request org markdown-mode shell-maker))

(provide 'init-copilot)
;;; init-copilot.el ends here
