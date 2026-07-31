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
                           :models '(qwen2.5-coder:7b qwen3.5:9b))   ;; 4060
        gptel-model 'qwen3.5:9b))
(use-package claude-code-ide
  :ensure (:type git :host github :repo "manzaltu/claude-code-ide.el")
  ;; :preface so the command is defined by the time :bind runs. Both callees are
  ;; autoloaded, so pressing the key still loads the package on demand.
  :preface
  (defun ju/claude-code-ide-dwim ()
    "Toggle the Claude window for this project, starting a session if none.
`claude-code-ide-toggle' is a pure toggle and errors out when the project
has no session yet, which makes for a poor single keybinding."
    (interactive)
    (condition-case nil
        (claude-code-ide-toggle)
      (user-error (claude-code-ide))))
  :bind (("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
         ("<f8>" . ju/claude-code-ide-dwim))
  :config
  (setq claude-code-ide-terminal-backend 'ghostel)
  (claude-code-ide-emacs-tools-setup)
  (add-to-list 'display-buffer-alist
               '("\\*claude" (display-buffer-reuse-window display-buffer-below-selected)
                 (inhibit-switch-frame . t)
                 (reusable-frames . nil)))

  ;; claude-code-ide sessions remember their tab-bar tab but not their frame,
  ;; so an incoming MCP request (openDiff -> ediff, openFile, ...) is handled in
  ;; whichever frame happens to be selected. With one daemon serving two frames
  ;; on different projects, the diff lands in the wrong frame -- and openDiff
  ;; deletes that frame's side windows on the way. Pin each session to the frame
  ;; it was started in.
  (defvar ju/claude-ide--session-frames (make-hash-table :test 'equal)
    "Map of project directory -> frame the claude-code-ide session started in.")

  (defun ju/claude-ide--record-frame (orig-fn &optional project-directory)
    "Remember the current frame for this session before starting it."
    (puthash (expand-file-name (or project-directory default-directory))
             (selected-frame) ju/claude-ide--session-frames)
    (funcall orig-fn project-directory))

  (defun ju/claude-ide--forget-frame (orig-fn project-dir)
    (remhash (expand-file-name project-dir) ju/claude-ide--session-frames)
    (funcall orig-fn project-dir))

  (defun ju/claude-ide--in-session-frame (orig-fn message &optional session)
    "Handle MESSAGE inside the frame SESSION was started in, when it still lives."
    (let* ((dir (and session (claude-code-ide-mcp-session-project-dir session)))
           (frame (and dir (gethash dir ju/claude-ide--session-frames))))
      (if (and (frame-live-p frame) (not (eq frame (selected-frame))))
          (with-selected-frame frame (funcall orig-fn message session))
        (funcall orig-fn message session))))

  (advice-add 'claude-code-ide-mcp-start :around #'ju/claude-ide--record-frame)
  (advice-add 'claude-code-ide-mcp-stop-session :around #'ju/claude-ide--forget-frame)
  (advice-add 'claude-code-ide-mcp--handle-message :around #'ju/claude-ide--in-session-frame))

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
