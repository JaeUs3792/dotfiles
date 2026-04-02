;; init-ai -*- lexical-binding: t -*-
(use-package gptel
  :ensure t
  :defer t
  :config
  (setq gptel-model "gpt-4o-mini")
  (setq gptel-api-key custom-chatgpt-my-key)
  (setq gptel-backend
        (gptel-make-ollama "Ollama"
                           :host "localhost:11434"
                           :stream t
                          ;; :models '(qwen3-coder:30b gemma3:27b)) ;; 5080
        ;; gptel-model 'gemma3:27b))
                           :models '(qwen2.5-coder:7b qwen3:8b))   ;; 4060
        gptel-model 'qwen3:8b))

(use-package claude-code
  :ensure (:host github :repo "stevemolitor/claude-code.el")
  :config
  (claude-code-mode)
  (setq claude-code-toggle-auto-select t))

(ju/leader-key-def
  "a" '(:ignore t :which-key "AI")
  ;; gptel
  "a a" '(gptel :which-key "open chat")
  "a s" '(gptel-send :which-key "send")
  "a m" '(gptel-menu :which-key "menu")
  "a b" '(gptel-set-backend :which-key "set backend")
  "a r" '(gptel-rewrite :which-key "rewrite region")
  ;; claude-code
  "a l" '(:ignore t :which-key "claude-code")
  "a l l" '(claude-code :which-key "start")
  "a l c" '(claude-code-continue :which-key "continue session")
  "a l s" '(claude-code-send-command :which-key "send command")
  "a l r" '(claude-code-send-region :which-key "send region")
  "a l o" '(claude-code-send-buffer-file :which-key "send buffer file")
  "a l e" '(claude-code-fix-error-at-point :which-key "fix error at point")
  "a l t" '(claude-code-toggle :which-key "toggle window")
  "a l b" '(claude-code-switch-to-buffer :which-key "switch to buffer")
  "a l k" '(claude-code-kill :which-key "kill session"))

(provide 'init-ai)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ai.el ends here
