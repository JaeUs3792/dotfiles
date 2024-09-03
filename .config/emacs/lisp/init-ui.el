;;; init-ui.el -*- lexical-binding: t -*-
(require 'init-const)
(require 'init-funcs)

;; Optimization
(setq idle-update-delay 1.0)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Make certain buffers grossly incandescent
(use-package solaire-mode
  :straight t
  :ensure t
  :demand t
  :config
  (solaire-global-mode +1))

(use-package doom-themes
  :straight t
  :ensure t
  :init
  (if (display-graphic-p)
      (load-theme custom-theme-sel t)
    (load-theme 'doom-gruvbox t))
  :config
  (doom-themes-visual-bell-config))
(use-package doom-modeline
  :straight t
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-window-width-limit 110
        doom-modeline-minor-modes t)
  :config
  (setq doom-modeline-height 15
        doom-modeline-env-version t
        doom-modeline-persp-name t
        doom-modeline-persp-icon t
        doom-modeline-display-default-persp-name t
        doom-modeline-indent-info t))
;;(use-package hide-mode-line
;;  :hook (((completion-list-mode
;;           completion-in-region-mode
;;           eshell-mode shell-mode
;;           term-mode vterm-mode
;;           treemacs-mode
;;           lsp-ui-imenu-mode
;;           pdf-annot-list-mode) . hide-mode-line-mode)))

;; A minor-mode menu for mode-line
(use-package minions
  :straight t
  :ensure t
  :hook (doom-modeline-mode . minions-mode))

;; Icons
(use-package nerd-icons
  :straight t
  :ensure t)
;;(use-package all-the-icons)

(use-package display-line-numbers
  :straight t
  :ensure t
  :hook ((prog-mode yaml-mode conf-mode) . display-line-numbers-mode)
  :init (setq display-line-numbers-width-start t))

;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen nil
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-scratch-message nil)
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; Display dividers between windows
(setq window-divider-default-places t
      window-divider-default-bottom-width 5
      window-divider-default-right-width 5)
(add-hook 'window-setup-hook #'window-divider-mode)

(use-package time
  :straight t
  :ensure t
  :init (setq display-time-24hr-format t
              display-time-day-and-date t))

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
        mouse-wheel-scroll-amount-horizontal 1
        mouse-wheel-progressive-speed nil))
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000
      auto-window-vscroll nil
      scroll-preserve-screen-position t)

;; Good pixel line scrolling
(if (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode t)
    (use-package good-scroll
      :straight t
      :ensure t
      :hook (after-init . good-scroll-mode)
      :bind (([remap next] . good-scroll-up-full-screen)
             ([remap prior] . good-scroll-down-full-screen))))

;; Smooth scrolling over images
(use-package iscroll
  :straight t
  :ensure t
  :defer t
  :hook (image-mode . iscroll-mode))

;; Use fixed pitch where it's sensible
(use-package mixed-pitch
  :straight t
  :ensure t
  :defer t)

;; Display ugly ^L page breaks as tidy horizontal lines
(use-package page-break-lines
  :straight t
  :ensure t
  :defer t
  :diminish
  :hook (after-init . global-page-break-lines-mode)
  :config ;; display only half fix.
  (set-fontset-font "fontset-default"
                    (cons page-break-lines-char page-break-lines-char)
                    (face-attribute 'default :family)))
(use-package form-feed
  :disabled
  :hook (after-init . global-form-feed-mode))

;; emoji
(use-package emojify
  :straight t
  :ensure t
  :defer t
  :init (global-emojify-mode 1))

(provide 'init-ui)
;;; init-ui.el ends here.
