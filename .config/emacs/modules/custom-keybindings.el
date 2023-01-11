(global-set-key (kbd "C-c t") 'toggle-transparency)

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
  "." 'find-file
  ;; Buffer
  "b" '(:ignore t :which-key "buffer handling")
  "b i" '(ibuffer :which-key "IBuffer")
  "b r" '(revert-buffer :which-key "Revert Buffer")
  "b k" '(kill-current-buffer :which-key "Kill current buffer")
  "b n" '(next-buffer :which-key "Next buffer")
  "b p" '(previous-buffer :which-key "Previous buffer")
  "b B" '(ibuffer-list-buffers :which-key "IBuffer List Buffers")
  "b K" '(kill-buffer :which-key "IBuffer Kill Buffers")
  ;; Eshell
  "e" '(:ignore t :which-key "eshell")
  "e h" '(counsel-esh-history :which "Kill history")
  "e s" '(eshell :which "run eshell")
  ;; Workspace
  ;; Counsel
  "f" '(:ignore t :which-key "file op.")
  "f r" '(consult-recent-file :which-key "Recent files")
  "t t" '(toggle-truncate-lines :which-key "Toggle truncate lines")
  ;; Shortcut
  "f o d" '((lambda () (interactive) (find-file (expand-file-name "~/.config/emacs/desktop.org"))) :which-key "open exwm config")
  "f o p" '((lambda () (interactive) (find-file (expand-file-name "~/org/example/emacs_my_previous.org"))) :which-key "open exwm config")
  "f o e" '((lambda () (interactive) (find-file (expand-file-name "~/org/example/emacs_another.org"))) :which-key "open exwm config")
  "f o c" '((lambda () (interactive) (find-file (expand-file-name "~/.config/emacs/emacs.org"))) :which-key "open emacs config")
  ;; Hydra
  "h" '(:ignore t :which-key "hydra")
  "h t" '(hydra-text-scale/body :which-key "scale text")
  "h w" '(hydra-writeroom-scale/body :which-key "scale whiteroom")
  "h a" '(hydra-modify-alpha/body :which-key "modify alpha background"))

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
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))
(use-package undo-tree
  :init
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-diff       t
        undo-tree-visualizer-timestamps t)
        undo-tree-auto-save-history     t
        undo-tree-enable-undo-in-region t
        undo-limit        (* 800 1024)
        undo-strong-limit (* 12 1024 1024)
        undo-outer-limit  (* 128 1024 1024))

(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("t" text-scale-increase "in")
  ("s" text-scale-decrease "out")
  ("q" nil "finished" :exit t))
(defhydra hydra-writeroom-scale (:timeout 4)
  "scale whiteroom"
  ("t" writeroom-increase-width "enlarge")
  ("s" writeroom-decrease-width "shrink")
  ("r" writeroom-adjust-width "adjust")
  ("q" nil "finished" :exit t))
(defhydra hydra-modify-alpha ()
  ("q" nil :exit t)
  ("s" my/decrease-frame-alpha-background)
  ("t" my/increase-frame-alpha-background))

(provide 'custom-keybindings)
;;; custom-keybindings.el ends here
