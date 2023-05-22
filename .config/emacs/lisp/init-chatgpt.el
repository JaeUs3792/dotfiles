;; init-chatgpt.el -*- lexical-binding: t -*-
(use-package chatgpt-shell
  :config
  (setq chatgpt-shell-openai-key custom-chatgpt-my-key))
(use-package dall-e-shell
  :config
  (setq dall-e-shell-openai-key custom-chatgpt-my-key))

(provide 'init-chatgpt)
;;; init-chatgpt.el ends here
