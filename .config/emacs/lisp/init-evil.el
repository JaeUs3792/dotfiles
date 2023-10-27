;;; init-evil.el -*- lexical-binding: t -*-
(require 'init-const)
(require 'init-funcs)

(use-package evil
  :ensure nil
  :diminish
  :hook (after-init . evil-mode)
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil)
  ;;(setq evil-respect-visual-line-mode nil) ; t : on the screen, nil : by cr characters
  ;; Make evil search more like vim (makes emacs slow)
  ;;(evil-select-search-module 'evil-search-module 'evil-search)

  :config
  (require 'evil-vars)
  (evil-set-undo-system 'undo-tree)
  ;;(evil-set-undo-system 'undo-redo)
  (setq evil-want-fine-undo t) ; more granular undo with evil
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  ;; eshell no evil
  (dolist (mode '(eshell-mode))
    (add-to-list 'evil-emacs-state-modes mode)))
(with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol)
    ;; make evil-search-word look for symbol rather than word boundaries
    (setq-default evil-symbol-word-search t))
(use-package evil-collection
  :defer nil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :defer nil
  :config
  ;; evil nerd commenter
  (define-key evil-normal-state-map (kbd "g c") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-visual-state-map (kbd "g c") 'evilnc-comment-or-uncomment-lines))

(use-package evil-numbers
  :defer nil
  :config
  ;; evil numbers
  ;; unfortunately C-x is emacs common key binding.
  (define-key evil-normal-state-map (kbd "g =") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "g -") 'evil-numbers/dec-at-pt)
  (define-key evil-visual-state-map (kbd "g =") 'evil-numbers/inc-at-pt)
  (define-key evil-visual-state-map (kbd "g -") 'evil-numbers/dec-at-pt))

(provide 'init-evil)
;;; init-evil.el ends here.
