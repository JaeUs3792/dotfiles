;; init-chatgpt.el -*- lexical-binding: t -*-
(use-package gptel
  :straight t
  :ensure t
  :defer t
  :config
  (setq gptel-model "gpt-4o-mini")
  (setq gptel-api-key custom-chatgpt-my-key))


(provide 'init-chatgpt)
;;; init-chatgpt.el ends here
