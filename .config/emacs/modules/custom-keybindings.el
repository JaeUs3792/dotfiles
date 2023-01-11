(use-package which-key
  :defer t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

(use-package general
  :init
  (general-auto-unbind-keys)
  :config
  (general-evil-setup t)
  (general-create-definer ju/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(ju/leader-key-def
 "." 'find-file)
 ;; Buffer)

(use-package evil
  :after (general)
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil)
  (require 'evil-vars)
  (evil-set-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  (setq evil-want-fine-undo t) ; more granular undo with evil
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(provide 'custom-keybindings)
;;; custom-keybindings.el ends here
