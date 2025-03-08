;; init-chatgpt.el -*- lexical-binding: t -*-
(use-package gptel
  :straight t
  :ensure t
  :defer t
  :config
  (setq gptel-model "gpt-4o-mini")
  (setq gptel-api-key custom-chatgpt-my-key))

(use-package org-ai
  :straight t
  :ensure t
  :commands (org-ai-mode
             org-ai-global-mode)
  :init
  (add-hook 'org-mode-hook #'org-ai-mode) ; enable org-ai in org-mode
  (org-ai-global-mode) ; installs global keybindings on C-c M-a
  :config
  (setq org-ai-default-chat-model "gpt-4") ; if you are on the gpt-4 beta:
  (org-ai-install-yasnippets)) ; if you are using yasnippet and want `ai` snippets

(provide 'init-chatgpt)
;;; init-chatgpt.el ends here
