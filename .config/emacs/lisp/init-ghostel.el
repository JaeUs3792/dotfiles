;;; init-ghostel.el -*- lexical-binding: t -*-
;; ghostel: terminal emulator based on libghostty-vt (Ghostty's VT engine);
;; native module is auto-downloaded on first use
(use-package ghostel
  :ensure t
  :defer t)

;; Lisp input methods (korean-hangul) commit by calling `self-insert-command'
;; directly, bypassing ghostel's [remap self-insert-command] -> the syllable
;; lands in the buffer but never reaches the PTY. ghostel-ime-mode wraps
;; `input-method-function' to forward the commit instead.
;; Ships inside the ghostel repo, so no separate :ensure.
(use-package ghostel-ime
  :ensure nil
  :hook (ghostel-mode . ghostel-ime-mode))

;; popterm: pop-up terminal toggler (posframe/window/fullscreen) with
;; per-project instances
(use-package popterm
  :ensure t
  :commands (popterm-toggle popterm-toggle-cd popterm-toggle-named popterm-find)
  :config
  (setq popterm-backend        'ghostel
        popterm-display-method 'window
        popterm-scope          'project
        popterm-auto-cd        t)
  (popterm-global-mode 1))

(provide 'init-ghostel)
;;; init-ghostel.el ends here
